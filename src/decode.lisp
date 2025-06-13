(defpackage :simple-json-parser
  (:nicknames :sjp)
  (:use #:cl)
  (:export #:decode #:encode #:decode-event #:define-json-decoder))

(in-package :simple-json-parser)

(declaim (optimize (speed 3) (safety 0)))
(declaim (inline clear-buffer add-buffer new-buffer new-hex-buffer if-eof-error eof-p move-next))

(declaim (type fixnum +max-nesting+ *pos* *level*))
(declaim (type (vector character) *buffer*))
(declaim (type (simple-array character (4)) *hex-buffer*))
(declaim (type character *current*))
(declaim (type (function () (values character fixnum)) *next-char*))

(defun new-buffer () (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0))
(defun new-hex-buffer () (make-array 4 :element-type 'character))

(defconstant +max-nesting+ 64)
(defparameter *next-char* (lambda () (values #\Nul -1)))
(defparameter *current* #\Nul)
(defparameter *pos* 0)
(defparameter *level* 0)
(defparameter *buffer* (new-buffer))
(defparameter *hex-buffer* (new-hex-buffer))

(defun eof-p ()
  (char= #\Nul *current*))

(defun clear-buffer ()
  (setf (fill-pointer *buffer*) 0))

(defun add-buffer (c)
  (declare (type character c))
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
  (error (format nil "At pos ~A, ~A~%" *pos* msg)))

(defun if-eof-error (msg)
  (if (eof-p)
      (json-error msg)))

(defun move-next ()
  (setf (values *current* *pos*) (funcall *next-char*)))

(defun type-from-char ()
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
    (#\Nul :eof)
    (otherwise :illegal)))

(defun decode-constant (type)
  (declare (type symbol type))
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
  (loop while (and (not (eof-p)) (not (char= #\" *current*)))
	do (case *current*
	     (#\\
	      (move-next)
	      (if-eof-error "expecting char")
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
  
  (if-eof-error "expected \" character, got eof")
  
  (move-next)
  (copy-seq *buffer*))

(defun decode-array ()
  (move-next)
  (skip-whitespace)
  (loop while (and *current* (not (char= #\] *current*)))
	collecting (decode-element) into array-list
	summing 1 into array-size fixnum
	do (skip-whitespace)
	   (if-eof-error "expecting ',' or ']' got eof")
	   (case *current*
	     (#\,
	      (move-next)
	      (skip-whitespace)
	      (if-eof-error(format nil "expecting array element got ~A" *current*))
	      (if (char= #\] *current*)
		  (json-error "expecting array element got ']'")))
	     (#\] nil)
	     (otherwise (json-error "expecting legal json element")))
	finally (return (progn (move-next) (make-array array-size :initial-contents array-list))))) 

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
	   (if-eof-error "expecting ':' but got eof")
	   (if (not (eq :colon (type-from-char)))
	       (json-error (format nil "expecting ':' but got ~A" *current*)))
	   (move-next)
	   (setf current-val (decode-element))
	   (setf (gethash current-key table) current-val)
	   (skip-whitespace)
	   (if-eof-error "expecting ',' or '}' but got eof")
	   (case *current*
	     (#\, (move-next))
	     (#\} nil)
	     (otherwise (json-error (format nil "expecting ',' or '}' but got ~A" *current*))))
	finally (return (progn (move-next) table))))

(defun skip-whitespace ()
  (loop while (eq :whitespace (type-from-char))
	do (move-next)))

(defun decode-element ()
  (if (= (incf *level*) +max-nesting+)
      (json-error (format nil "nesting level is >= ~A" +max-nesting+)))
  
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
	  (if (not (eof-p))
	      (json-error "expecting eof"))))
    ret))

(defun next-char-from-stream (stm)
  (let ((pos -1))
    (declare (type fixnum pos))
    (lambda ()
      (values (read-char stm nil #\Nul) (incf pos)))))

(defmacro next-char-from-string-body (str)
  `(let ((pos -1)
	 (str-length (array-dimension ,str 0)))
     (declare (type fixnum pos str-length))
     (lambda ()
       (if (< (incf pos) str-length)
	   (values (aref ,str pos) pos)
	   (values #\Nul pos)))))

(defun next-char-from-simple-string (str)
  (declare (type simple-string str))
  (next-char-from-string-body str))

(defun next-char-from-string (str)
  (declare (type string str))
  (next-char-from-string-body str))

(defun decode (source)
  (let ((*next-char* (etypecase source
		       (stream (next-char-from-stream source))
		       (simple-string (next-char-from-simple-string source))
		       (string (next-char-from-string source))))
	(*current* #\Nul)
	(*level* 0)
	(*buffer* (new-buffer))
	(*hex-buffer* (new-hex-buffer)))
    (move-next)
    (decode-element)))
