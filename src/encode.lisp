(in-package :simple-json-parser)

(declaim (inline char-out str-out fmt-out write-string-element))

(defparameter *target* nil)
(defparameter *pretty* nil)
(defparameter *indent* 2)
(defparameter *level* 0)

(defun char-out (c)
  (write-char c *target*))

(defun str-out (s)
  (write-string s *target*))

(defun fmt-out (template obj)
  (format *target* template obj))

(defun write-string-element (str)
  (char-out #\")
  (str-out str)
  (char-out #\"))

(defun write-array-element (object)
  (char-out #\[)
  (loop with last-idx = (1- (length object))
	for element across object
	for idx from 0 to (length object)
	do (write-element element)
	   (if (not (= last-idx idx))
	       (char-out #\,)))
  (char-out #\]))

(defun write-hash-table-element (object)
  (char-out #\{)
  (loop with last-idx = (1- (hash-table-count object))
	for key being the hash-keys in object using (hash-value val)
	for idx from 0 to (hash-table-count object)
	do (write-string-element key)
	   (char-out #\:)
	   (write-element val)
	   (if (not (= idx last-idx))
	       (char-out #\,)))
  (char-out #\}))

(defun write-element (object)
  (etypecase object
    (symbol
     (ecase object
       (:null (str-out "null"))
       (:true (str-out "true"))
       (:false (str-out "false"))))
    
    (float
     (fmt-out "~,,,,,,'eE" object))
    
    (integer
     (fmt-out "~D" object))
    
    (string
     (write-string-element object))
    
    (array
     (write-array-element object))
    
    (hash-table
     (write-hash-table-element object))))

(defun encode (object target &key (pretty nil) (indent 2))
  (let ((*pretty* pretty)
	(*indent* indent)
	(*level* 0))
    (if (null target)
	(let ((str (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
	  (with-output-to-string (stm str)
	    (let ((*target* stm))
	      (write-element object)
	      str)))
	(let ((*target* target))
	  (write-element object)
	  *target*))))

