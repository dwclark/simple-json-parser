(defpackage :simple-json-parser-tests
  (:use #:cl #:fiveam #:simple-json-parser))

(in-package :simple-json-parser-tests)

(def-suite sjp-tests)
(in-suite sjp-tests)

(defparameter *resources* (asdf:system-relative-pathname "simple-json-parser-tests" "test/resources"))

(defun get-test-resource (s)
  (format nil "~A/~A" *resources* s))
  
(test test-decode-constants
  (is (eq :true (decode "true")))
  (is (eq :false (decode "false")))
  (is (eq :null (decode "null"))))

(test test-decode-numbers
  (is (= 1 (decode "1")))
  (is (= 123 (decode "123")))
  (is (equalp 1e10 (decode "1e10")))
  (is (equalp 1.2e10 (decode "1.2e10")))
  (is (equalp 1.2e10 (decode "1.2e+10")))
  (is (= 1.257 (coerce (decode "1.257") 'single-float)))
  (is (= 1.245d-9 (decode "12.45e-10")))
  (is (= 1.783456e-10 (coerce (decode "178.3456e-12") 'single-float)))
  (is (= -1.783456e-10 (coerce (decode "-178.3456e-12") 'single-float)))
  (is (zerop (decode "0")))
  (is (zerop (decode "-0"))))

(test test-decode-string
  (is (string= "foo" (decode "\"foo\"")))
  (is (string= (decode "\"fo\\no\"") (make-array 4 :element-type 'character :initial-contents '(#\f #\o #\Linefeed #\o))))
  (is (string= (decode "\"t\\tab\"") (make-array 4 :element-type 'character :initial-contents '(#\t #\Tab #\a #\b))))
  (is (string= "foo" (decode "\"\\u0066\\u006f\\u006f\"")))
  (is (string= "foo" (decode "\"\\u0066\\u006F\\u006F\""))))

(test test-decode-array
  (is (equalp #() (decode "[]")))
  (is (equalp #() (decode "[         ]")))
  (is (equalp #(1 2 3) (decode "[1,2,3]")))
  (is (equalp #(1 2 3) (decode "[ 1  ,2   ,   3]")))
  (is (equalp #(1 2 #(3 4)) (decode "[1,2,[3,4]]")))
  (is (equalp #(1 2 #(3 4)) (decode "[1  ,   2,[    3,   4    ]]")))
  (is (equalp #("foo") (decode "[\"foo\"]")))
  (is (equalp #(" fo  o") (decode "[   \" fo  o\"    ]")))
  (is (equalp #(1 "two" #("buckle" 3) 4) (decode "[1, \"two\", [ \"buckle\" , 3  ], 4]"))))

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
  (is (expected-hash-table-p '() (decode "{}")))
  (is (expected-hash-table-p '(("foo" . 1)) (decode "{ \"foo\": 1 }")))
  (is (expected-hash-table-p '(("foo" . #(1 2 3)) ("1" . ("key" . 100)))
			     (decode "{ \"foo\": [1,2,3], \"1\": { \"key\": 100 } }"))))


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
      (is (string= "4" (gethash "answer" (gethash "q2" (gethash "maths" (gethash "quiz" parsed)))))))))
