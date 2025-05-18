(defpackage :simple-json-parser
  (:nicknames :sjp)
  (:use #:cl)
  (:export #:decode #:encode))

(in-package :simple-json-parser)

(defparameter *source* nil)
(defparameter *current* nil)
(defparameter *line* 0)
(defparameter *column* 0)
(defparameter *level* 0)
(defparameter *buffer* nil)
(defparameter *hex-buffer* nil)

(defun clear-buffer ()
  (setf (fill-pointer *buffer*) 0))

(defun add-buffer (c)
  (vector-push-extend c *buffer*))

(defun hex-code ()
  (loop for idx from 0 to 3
	do (if (and *current* (digit-char-p *current* 16))
	       (progn
		 (setf (aref *hex-buffer* idx) *current*)
		 (move-next))
	       (json-error "expecting hex digit"))
	finally (return (parse-integer *hex-buffer* :radix 16))))

(defun json-error (msg)
  (error (format nil "At line ~A, column ~A: ~A~%" *line* *column* msg)))

(defun move-next ()
  (setf *current* (read-char *source* nil nil))
  (case *current*
    (#\Linefeed (incf *line*) (setf *column* 0))
    (otherwise (incf *column*))))

(defun type-from-char ()
  (if (null *current*)
      :eof
      (case *current*
	((#\Space #\Tab #\Return #\Linefeed) :whitespace)
	(#\, :comma)
	(#\: :colon)
	((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :number)
	(#\" :string)
	(#\{ :object)
	(#\[ :array)
	(#\t :true)
	(#\f :false)
	(#\n :null)
	(otherwise :illegal))))

(defun decode-constant (type)
  (flet ((expect (str)
	   (loop for c across str
		 do (if (char= c *current*)
			(move-next)
			(json-error (format nil "expecting ~A" str))))))
    (case type
      (:true (expect "true"))
      (:false (expect "false"))
      (:null (expect "null")))
    type))

;; strategy, if has a decimal point or exponential, treat it like a double float
;; otherwise, thread it like an integer
(defun decode-number ()
  (flet ((parse-digits ()
	   (clear-buffer)
	   (loop while (and *current* (digit-char-p *current*))
		 do (add-buffer *current*)
		    (move-next)
		 finally (return (if (not (zerop (length *buffer*)))
				     (parse-integer *buffer*)
				     nil)))))
    (let ((sign-mantissa #\+)
	  (whole 0)
	  (has-fractional nil)
	  (fractional 0)
	  (has-exponent nil)
	  (sign-exponent #\+)
	  (exponent 0))
      (if (eql #\- *current*)
	  (progn
	    (setf sign-mantissa #\-)
	    (move-next)))
      
      (setf whole (parse-digits))
      (if (null whole)
	  (json-error "no whole digits")) 
      
      (if (eql #\. *current*)
	  (progn
	    (move-next)
	    (setf fractional (parse-digits))
	    (if (null fractional)
		(json-error "no fractional digits")
		(progn
		  (setf has-fractional t)
		  (setf fractional (coerce (/ fractional (expt 10 (length *buffer*))) 'double-float))))))
      
      (if (or (eql #\e *current*) (eql #\E *current*))
	  (progn
	    (setf has-exponent t)
	    (move-next)
	    (case *current*
	      (#\+ (move-next))
	      (#\- (setf sign-exponent #\-) (move-next)))
	    (setf exponent (parse-digits))
	    (if (null exponent)
		(json-error "no exponent-digits"))))
      
      (if (and (not has-fractional) (not has-exponent))
	  whole
	  (let ((accum (coerce whole 'double-float)))
	    (if has-fractional
		(incf accum fractional))
	    (if has-exponent
		(setf accum (* accum (expt 10 (if (char= #\+ sign-exponent)
						  exponent
						  (- exponent))))))
	    (if (char= #\+ sign-mantissa)
		accum
		(- accum)))))))

(defun decode-string ()
  (move-next)
  (clear-buffer)
  (loop while (and *current* (not (char= #\" *current*)))
	do (case *current*
	     (#\\
	      (move-next)
	      (if (null *current*)
		  (json-error "expecting char"))
	      (case *current*
		(#\" (add-buffer #\") (move-next))
		(#\\ (add-buffer #\\) (move-next))
		(#\/ (add-buffer #\/) (move-next))
		(#\b (add-buffer #\Backspace) (move-next))
		(#\f (add-buffer #\Formfeed) (move-next))
		(#\n (add-buffer #\Linefeed) (move-next))
		(#\r (add-buffer #\Return) (move-next))
		(#\t (add-buffer #\Tab) (move-next))
		(#\u (move-next) (add-buffer (code-char (hex-code))))
		(otherwise (json-error "expected special char sequence"))))
	     (otherwise (add-buffer *current*) (move-next))))
  
  (if (null *current*)
      (json-error "expected \" character, got eof"))
  
  (move-next)
  (copy-seq *buffer*))

(defun decode-array ()
  (move-next)
  (skip-whitespace)
  (loop with ary = (make-array 0 :adjustable t :fill-pointer 0)
	while (and *current* (not (char= #\] *current*)))
	do (vector-push-extend (decode-stream) ary)
	   (skip-whitespace)
	   (if (null *current*)
	       (json-error "expecting ',' or ']' got eof"))
	   (case *current*
	     (#\,
	      (move-next)
	      (skip-whitespace)
	      (if (null *current*)
		  (json-error (format nil "expecting array element got ~A" *current*)))
	      (if (char= #\] *current*)
		  (json-error "expecting array element got ']'")))
	     (#\] nil)
	     (otherwise (json-error "expecting legal json element")))
	finally (return (progn (move-next) ary))))

(defun decode-object ()
  (move-next)
  (skip-whitespace)
  (loop with table = (make-hash-table :test #'equal)
	with current-key = nil
	with current-val = nil
	while (and *current* (not (char= #\} *current*)))
	do (skip-whitespace)
	   (if (not (eq :string (type-from-char)))
	       (json-error (format nil "expecting '\"' but got ~A" *current*)))
	   (setf current-key (decode-string))
	   (skip-whitespace)
	   (if (null *current*)
	       (json-error "expecting ':' but got eof"))
	   (if (not (eq :colon (type-from-char)))
	       (json-error (format nil "expecting ':' but got ~A" *current*)))
	   (move-next)
	   (setf current-val (decode-stream))
	   (setf (gethash current-key table) current-val)
	   (skip-whitespace)
	   (if (null *current*)
	       (json-error "expecting ',' or '}' but got eof"))
	   (case *current*
	     (#\, (move-next))
	     (#\} nil)
	     (otherwise (json-error (format nil "expecting ',' or '}' but got ~A" *current*))))
	finally (return (progn (move-next) table))))
	   

(defun skip-whitespace ()
  (loop while (eq :whitespace (type-from-char))
	do (move-next)))

(defun decode-stream ()
  (incf *level*)
  (skip-whitespace)
  (let* ((type (type-from-char))
	 (ret (case type
		(:string (decode-string))
		(:number (decode-number))
		(:object (decode-object))
		(:array (decode-array))
		((:true :false :null) (decode-constant type))
		(otherwise (json-error (format nil "illegal character ~A" *current*))))))
    (decf *level*)
    (if (= 0 *level*)
	(progn
	  (skip-whitespace)
	  (if (not (eq :eof (type-from-char)))
	      (json-error "expecting eof"))))
    ret))

(defun decode (source)
  (etypecase source
    (stream (let ((*source* source)
		  (*current* (read-char source nil nil))
		  (*line* 1)
		  (*column* 0)
		  (*level* 0)
		  (*buffer* (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
		  (*hex-buffer* (make-array 4 :element-type 'character)))
	      (decode-stream)))
    (string (with-input-from-string (stm source)
	      (decode stm)))))
