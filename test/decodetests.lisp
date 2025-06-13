(defpackage :simple-json-parser-tests
  (:use #:cl #:fiveam #:simple-json-parser))

(in-package :simple-json-parser-tests)

(def-suite sjp-tests)
(in-suite sjp-tests)

(defparameter *resources* (asdf:system-relative-pathname "simple-json-parser-tests" "test/resources"))
(defparameter *test-json-files* "/home/david/Sources/JSONTestSuite/test_parsing")

(define-json-decoder decode-event :type simple-string)
(define-json-decoder decode-from-stream :type stream)

(defun get-test-resource (s)
  (format nil "~A/~A" *resources* s))

(defun wrap-stream (s)
  (with-input-from-string (stm s)
    (decode-from-stream stm)))

(test test-decode-constants
  (loop for func in (list #'decode #'decode-event #'wrap-stream)
	do (is (eq :true (funcall func "true")))
	   (is (eq :false (funcall func "false")))
	   (is (eq :null (funcall func "null")))))

(test test-decode-numbers
  (loop for func in (list #'decode #'decode-event #'wrap-stream)
	do (is (= 1 (funcall func "1")))
	   (is (= 123 (funcall func "123")))
	   (is (equalp 1e10 (funcall func "1e10")))
	   (is (equalp 1.2e10 (funcall func "1.2e10")))
	   (is (equalp 1.2e10 (funcall func "1.2e+10")))
	   (is (= 1.257 (coerce (funcall func "1.257") 'single-float)))
	   (is (= 1.245d-9 (funcall func "12.45e-10")))
	   (is (= 1.783456e-10 (coerce (funcall func "178.3456e-12") 'single-float)))
	   (is (= -1.783456e-10 (coerce (funcall func "-178.3456e-12") 'single-float)))
	   (is (zerop (funcall func "0")))
	   (is (zerop (funcall func "-0")))))

(test test-decode-string
  (loop for func in (list #'decode #'decode-event #'wrap-stream)
	do (is (string= "foo" (funcall func "\"foo\"")))
	   (is (string= (funcall func "\"fo\\no\"") (make-array 4 :element-type 'character :initial-contents '(#\f #\o #\Linefeed #\o))))
	   (is (string= (funcall func "\"t\\tab\"") (make-array 4 :element-type 'character :initial-contents '(#\t #\Tab #\a #\b))))
	   (is (string= "foo" (funcall func "\"\\u0066\\u006f\\u006f\"")))
	   (is (string= "foo" (funcall func "\"\\u0066\\u006F\\u006F\"")))))

(test test-decode-array
  (loop for func in (list #'decode #'decode-event #'wrap-stream)
	do (is (equalp #() (funcall func "[]")))
	   (is (equalp #() (funcall func "[         ]")))
	   (is (equalp #(1 2 3) (funcall func "[1,2,3]")))
	   (is (equalp #(1 2 3) (funcall func "[ 1  ,2   ,   3]")))
	   (is (equalp #(1 2 #(3 4)) (funcall func "[1,2,[3,4]]")))
	   (is (equalp #(1 2 #(3 4)) (funcall func "[1  ,   2,[    3,   4    ]]")))
	   (is (equalp #("foo") (funcall func "[\"foo\"]")))
	   (is (equalp #(" fo  o") (funcall func "[   \" fo  o\"    ]")))
	   (is (equalp #(1 "two" #("buckle" 3) 4) (funcall func "[1, \"two\", [ \"buckle\" , 3  ], 4]")))))

(defun expected-hash-table-p (list table)
  (loop for (key . val) in list
	do (let ((hval (gethash key table)))
	     (cond
	       ((hash-table-p hval)
		(if (not (expected-hash-table-p (list val) hval))
		    (return-from expected-hash-table-p nil)))
	       (t (if (not (equalp val hval))
		      (return-from expected-hash-table-p nil)))))
	finally (return t)))

(test test-decode-object
  (loop for func in (list #'decode #'decode-event #'wrap-stream)
	do (is (expected-hash-table-p '() (funcall func "{}")))
	   (is (expected-hash-table-p '(("foo" . 1)) (funcall func "{ \"foo\": 1 }")))
	   (is (expected-hash-table-p '(("foo" . #(1 2 3)) ("1" . ("key" . 100)))
				      (funcall func "{ \"foo\": [1,2,3], \"1\": { \"key\": 100 } }")))))


(test test-file-parsing
  (with-open-file (fstream (get-test-resource "large1.json"))
    (let ((parsed (decode fstream)))
      (flet ((correct-p (table)
	       (and (= 22 (hash-table-count table))
		    (= 3 (length (gethash "friends" table))))))
	(is (every #'correct-p parsed)))))

  (flet ((correct-p (table)
	   (every #'(lambda (key)
		      (multiple-value-bind (key present) (gethash key table)
			present))
		  '("name" "language" "id" "bio" "version"))))

    (loop for file in '("128KB.json" "128KB-min.json")
	  do (with-open-file (fstream (get-test-resource "128KB.json"))
	       (let ((parsed (decode fstream)))
		 (is (vectorp parsed))
		 (is (= 788 (length parsed)))
		 (is (every #'correct-p parsed))))))

  (with-open-file (fstream (get-test-resource "fruit.json"))
    (let ((parsed (decode fstream)))
      (flet ((correct-p (cell)
	       (string= (cdr cell) (gethash (car cell) parsed))))
	(is (hash-table-p parsed))
	(is (= 3 (hash-table-count parsed)))
	(is (every #'correct-p '(("fruit" . "Apple") ("size" . "Large") ("color" . "Red")))))))

  (with-open-file (fstream (get-test-resource "quiz.json"))
    (let ((parsed (decode fstream)))
      (is (string= "12" (gethash "answer" (gethash "q1" (gethash "maths" (gethash "quiz" parsed))))))
      (is (string= "4" (gethash "answer" (gethash "q2" (gethash "maths" (gethash "quiz" parsed))))))))

  (with-open-file (fstream (get-test-resource "constants.json"))
    (let ((parsed (decode fstream)))
      (is (eq :null (gethash "null" parsed)))
      (is (eq :true (gethash "true" parsed)))
      (is (eq :false (gethash "false" parsed))))))

(test run-y-tests ()
  (dolist (path-name (directory (concatenate 'string *test-json-files* "/y*.json")))
    (with-open-file (fstream path-name)
      (let ((parsed (decode fstream)))
	(is (eq :true :true))))))

(test run-n-tests ()
  (dolist (path-name (directory (concatenate 'string *test-json-files* "/n*.json")))
    (with-open-file (fstream path-name)
      (handler-case
	  (progn
	    (decode fstream)
	    (is (eq :true :false)))
	(error (se)
	  (is (eq :true :true)))))))

;; Now getting around 42 MB/s
;; I thought this was good until I ran jzon on the same contents
;; It gets 100 MB/s consistently
;; I'm also impressed that it uses generics extensively
(defun test-performance ()
  (let ((contents (uiop:read-file-string (get-test-resource "128KB.json"))))
    (dotimes (myvar 1024)
      (decode contents))
    nil))

;; gets around 97 MB/s once you remove the CLOS stuff
;; gets around 102 MB/s when you also turn off array bounds checking
;; gets > 110 MB/s when you simplify current/next/prev/peek
(defun test-event-performance ()
  (let ((contents (uiop:read-file-string (get-test-resource "128KB.json"))))
    (dotimes (myvar 1024)
      (decode-event contents))
    nil))

