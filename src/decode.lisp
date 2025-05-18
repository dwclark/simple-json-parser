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

(defun clear-buffer ()
  (setf (fill-pointer *buffer*) 0))

(defun add-buffer (c)
  (vector-push-extend c *buffer*))

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
	  (fractional 0)
	  (has-exponent nil)
	  (sign-exponent #\+)
	  (exponent 0)
	  (accum 0))
      (if (eql #\- *current*)
	  (progn
	    (setf sign-mantissa #\-)
	    (move-next)))
      
      (setf whole (parse-digits))
      (if (null whole)
	  (json-error "no whole digits")) 
      
      (if (eql #\. *current*)
	  (let ((tmp 0))
	    (move-next)
	    (setf tmp (parse-digits))
	    (if (null tmp)
		(json-error "no fractional digits"))
	    (setf fractional (float (/ tmp (expt 10 (length *buffer*)))))))
      
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
      
      (setf accum whole)
      (incf accum fractional)
      (if has-exponent
	  (setf accum (* accum (expt 10 (if (char= #\+ sign-exponent)
					    exponent
					    (- exponent))))))
      (if (char= #\+ sign-mantissa)
	  accum
	  (- accum)))))

(defun decode-string ()
  (flet ((hex-code ()
	   (loop with hex-buffer = (make-array 4 :element-type 'character)
		 for idx from 0 to 3
		 do (if (and *current* (digit-char-p *current* 16))
			(progn
			  (setf (aref hex-buffer idx) *current*)
			  (move-next))
			(json-error "expecting hex digit"))
		 finally (return (parse-integer hex-buffer :radix 16)))))
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
    (copy-seq *buffer*)))

(defun decode-array ())

(defun decode-object ())

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
		((:true :false :null) (decode-constant type)))))
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
		  (*buffer* (make-array 1 :element-type 'character :adjustable t :fill-pointer 0)))
	      (decode-stream)))
    (string (with-input-from-string (stm source)
	      (decode stm)))))
