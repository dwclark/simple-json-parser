(in-package :simple-json-parser)

(defparameter *target* nil)

(defun encode (object target)
  (etypecase target
    (stream (let ((*target* target))
	      (encode-stream object)
	      target))
    (string (with-output-to-string (stream target)
	      (encode object stream)
	      target))))

(defun encode-stream (object))
