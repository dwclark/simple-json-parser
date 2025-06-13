(in-package :simple-json-parser)

;; inline functions
(declaim (inline str-parser-read-buffer str-parser-pos str-parser-move-next str-parser-current str-parser-next
		 stm-parser-read-buffer stm-parser-pos stm-parser-move-next stm-parser-current stm-parser-next stm-parser-reset
		 whitespace-p
		 json-stack-pop-item json-stack-push json-stack-at json-stack-pop json-stack-push-array
		 json-stack-push-object json-stack-pop-array json-stack-pop-object
		 json-stack-validate-key json-stack-contents json-stack-location))

(defstruct str-parser
  (read-buffer "" :type simple-string)
  (pos 0 :type fixnum))

(defun str-parser-move-next (parser)
  (declare (type str-parser parser))
  (incf (str-parser-pos parser)))
  
(defun str-parser-current (parser)
  (declare (type str-parser parser))
  (if (< (str-parser-pos parser) (array-total-size (str-parser-read-buffer parser)))
      (schar (str-parser-read-buffer parser) (str-parser-pos parser))
      #\Nul))

(defun str-parser-next (parser)
  (declare (type str-parser parser))
  (if (< (incf (str-parser-pos parser)) (array-total-size (str-parser-read-buffer parser)))
      (schar (str-parser-read-buffer parser) (str-parser-pos parser))
      #\Nul))

(defstruct stm-parser
  (read-buffer (make-string 512) :type simple-string)
  (pos -1 :type fixnum)
  (stm nil :type stream))

(defun stm-parser-current (parser)
  (declare (type stm-parser parser))
  (schar (stm-parser-read-buffer parser) (stm-parser-pos parser)))

(defun stm-parser-move-next (parser)
  (declare (type stm-parser parser))
  (when (= (1+ (stm-parser-pos parser)) (array-total-size (stm-parser-read-buffer parser)))
    (loop with old of-type simple-string = (stm-parser-read-buffer parser)
	  with new of-type simple-string = (make-string (+ 512 (array-total-size old)))
	  for i from 0 below (array-total-size old)
	  do (setf (schar new i) (schar old i))
	  finally (setf (stm-parser-read-buffer parser) new)))
  (incf (stm-parser-pos parser))
  (setf (schar (stm-parser-read-buffer parser) (stm-parser-pos parser))
	(read-char (stm-parser-stm parser) nil #\Nul)))
  
(defun stm-parser-next (parser)
  (declare (type stm-parser parser))
  (stm-parser-move-next parser))

(defun stm-parser-reset (parser)
  (declare (type stm-parser parser))
  (setf (schar (stm-parser-read-buffer parser) 0) (stm-parser-current parser))
  (setf (stm-parser-pos parser) 0))
   
(defun whitespace-p (c)
  (declare (type character c))
  (case c
    ((#\Space #\Tab #\Return #\Linefeed) t)
    (otherwise nil)))

(defstruct json-stack
  (contents (make-array 1024) :type (simple-vector 1024))
  (location -1 :type fixnum))

(defun json-stack-push (stack item)
  (declare (type json-stack stack))
  (setf (aref (json-stack-contents stack) (incf (json-stack-location stack))) item))

(defun json-stack-at (stack i &optional (item nil item-supplied-p))
  (declare (type json-stack stack))
  (declare (type fixnum i))
  (if item-supplied-p
      (setf (aref (json-stack-contents stack) (+ i (json-stack-location stack))) item)
      (aref (json-stack-contents stack) (+ i (json-stack-location stack)))))

(defun json-stack-pop (stack)
  (declare (type json-stack stack))
  (let ((ret (json-stack-at stack 0)))
    (json-stack-at stack 0 nil)
    (decf (json-stack-location stack))
    ret))

(defun json-stack-push-array (stack)
  (declare (type json-stack stack))
  (let* ((new-array (make-array 0 :adjustable t :fill-pointer 0))
	 (cell (cons new-array nil)))
    (json-stack-push stack cell)))

(defun json-stack-push-object (stack)
  (declare (type json-stack stack))
  (let* ((new-object (make-hash-table :test #'equal))
	 (cell (cons new-object nil)))
    (json-stack-push stack cell)))

(defun json-stack-pop-item (stack)
  (declare (type json-stack stack))
  (let ((top (json-stack-pop stack))
	(array-cell (json-stack-at stack 0)))
    (if (consp array-cell)
	(if (typep (car array-cell) 'array)
	    (let ((array (car array-cell)))
	      (vector-push-extend top array)
	      (return-from json-stack-pop-item))
	    (error "expected array element, found something else")))
    
    (let ((value top)
	  (key (json-stack-pop stack))
	  (table-cell (json-stack-at stack 0)))
      (if (consp table-cell)
	  (if (typep (car table-cell) 'hash-table)
	      (let ((table (car table-cell)))
		(setf (gethash key table) value))
	      (error "expected object element, found something else"))))))

(defun json-stack-pop-array (stack)
  (declare (type json-stack stack))
  (let ((top-item (json-stack-at stack 0)))
    (if (atom top-item)
	(json-stack-pop-item stack))
    (json-stack-at stack 0 (car (json-stack-at stack 0)))))

(defun json-stack-pop-object (stack)
  (declare (type json-stack stack))
  (let ((top-item (json-stack-at stack 0)))
    (if (atom top-item)
	(json-stack-pop-item stack))
    (json-stack-at stack 0 (car (json-stack-at stack 0)))))

(defun json-stack-validate-key (stack)
  (declare (type json-stack stack))
  (if (not (typep (json-stack-at stack 0) 'string))
      (error "invalid key type")))

(defun json-string-unencode (s start end)
  (declare (type simple-string s))
  (declare (type fixnum start end))
  (loop with ret = (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
	with index = start
	while (< index end)
	do (let ((current (char s index)))
	     (incf index)
	     (case current
	       (#\\
		(let ((next (char s index)))
		  (incf index)
		  (case next
		    (#\" (vector-push-extend #\" ret))
		    (#\\ (vector-push-extend #\\ ret))
		    (#\/ (vector-push-extend #\/ ret))
		    (#\b (vector-push-extend #\Backspace ret))
		    (#\f (vector-push-extend #\Formfeed ret))
		    (#\n (vector-push-extend #\Linefeed ret))
		    (#\r (vector-push-extend #\Return ret))
		    (#\t (vector-push-extend #\Tab ret))
		    (#\u (vector-push-extend (code-char (parse-integer s :start (1+ index) :end (incf index 4)
									 :radix 16 :junk-allowed nil)) ret))
		    (otherwise (error "expected special char sequence")))))
	       (otherwise (vector-push-extend current ret))))
	finally (return ret)))

(defmacro define-json-decoder (name &key (type 'simple-string))
  (let ((type-expr (case type
		     (simple-string 'str-parser)
		     (stream 'stm-parser)))

	(construct-expr (case type
			  (simple-string `(make-str-parser :read-buffer src :pos 0))
			  (stream `(let ((tmp (make-stm-parser :stm src)))
				     (stm-parser-move-next tmp)
				     tmp))))
	(pos-expr (case type
		    (simple-string `(str-parser-pos my-parser))
		    (stream `(stm-parser-pos my-parser))))
	
	(read-buffer-expr (case type
			    (simple-string `(str-parser-read-buffer my-parser))
			    (stream `(stm-parser-read-buffer my-parser))))
	
	(reset-buffer-expr (case type
			     (simple-string 'nil)
			     (stream `(stm-parser-reset my-parser))))
	
	(move-next-expr (case type
			  (simple-string `(str-parser-move-next my-parser))
			  (stream `(stm-parser-move-next my-parser))))
			
	(current-expr (case type
			(simple-string `(str-parser-current my-parser))
			(stream `(stm-parser-current my-parser))))
	
	(next-expr (case type
		     (simple-string `(str-parser-next my-parser))
		     (stream `(stm-parser-next my-parser)))))
    
    `(defun ,name (src)
       (declare (optimize (speed 3) (debug 0) (safety 0)))
       (let* ((stack (make-json-stack))
	      (*read-default-float-format* 'double-float)
	      (my-parser ,construct-expr))

	 (declare (dynamic-extent stack my-parser))

	 ;; inline label functions
	 (declare (inline consume-string consume-number consume-whitespace consume-exact next-event))

	 (declare (type ,type-expr my-parser))
	 
	 (labels ((consume-number ()
		    (let ((start (the fixnum ,pos-expr))
			  (current (the character ,current-expr)))
		      
		      (when (or (char= #\+ current) (char= #\- current))
			(setf current ,next-expr))
		      
		      (when (not (digit-char-p current))
			(error "bare +/- sign found"))
		      
		      (loop while (digit-char-p (setf current ,next-expr)))
		      
		      (when (or (whitespace-p current) (char= current #\,) (char= current #\]) (char= current #\}) (char= current #\Nul))
			(return-from consume-number (values :integer start (1- ,pos-expr))))
		      
		      (when (char= #\. current)
			(setf current ,next-expr)
			(if (not (digit-char-p current))
			    (error "expected digits")))
		      
		      (loop while (digit-char-p (setf current ,next-expr)))
		      
		      (when (or (whitespace-p current) (char= current #\,) (char= current #\]) (char= current #\}) (char= current #\Nul))
			(return-from consume-number (values :float start (1- ,pos-expr))))
		      
		      (when (not (char-equal #\e current))
			(error "expected 'e' character"))
		      
		      (setf current ,next-expr)
		      (when (or (char= #\+ current) (char= #\- current))
			(setf current ,next-expr)
			(if (not (digit-char-p current))
			    (error "expected digits")))
		      
		      (loop while (digit-char-p (setf current ,next-expr)))
		      
		      (if (or (whitespace-p current) (char= current #\,) (char= current #\]) (char= current #\}) (char= current #\Nul))
			  (values :float start (1- ,pos-expr))
			  (error "expected digits"))))

		  (consume-string ()
		    (loop with start of-type fixnum = ,pos-expr
			  with end of-type fixnum = 0
			  with event-type = :string
			  with prev of-type character = #\Nul
			  for c of-type character = ,next-expr then ,next-expr
			  until (and (char= #\" c) (not (char= #\\ prev)))
			  do (when (char= #\\ c)
			       (setf c ,next-expr)
			       (case c
				 ((#\" #\\ #\/ #\b #\f #\n #\r #\t #\u) (setf event-type :escaped-string))
				 (otherwise (error "Expecting escape sequence"))))
			     (setf prev c)
			  finally (setf end ,pos-expr)
				  ,move-next-expr
				  (return (values event-type start end))))

		  (consume-whitespace ()
		    (loop for c of-type character = ,current-expr then ,next-expr
			  while (whitespace-p c)
			  finally (return c)))

		  (consume-exact (expect)
		    (declare (type simple-string expect))
		    (loop for c across expect
			  do (if (not (char= c ,next-expr))
				 (error (format t "expected ~A" c)))
			  finally ,move-next-expr))

		  (next-event ()
		    ;;(format t "in next-event, parser is ~A ~%" my-parser)
		    (ecase (consume-whitespace)
		      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (consume-number))
		      (#\" (consume-string))
		      (#\t
		       (consume-exact "rue")
		       (values :true -1 -1))
		      (#\f
		       (consume-exact "alse")
		       (values :false -1 -1))
		      (#\n
		       (consume-exact "ull")
		       (values :null -1 -1))
		      
		      (#\, ,move-next-expr (values :end-item -1 -1))
		      (#\: ,move-next-expr (values :end-key -1 -1))      
		      (#\{ ,move-next-expr (values :start-object -1 -1))
		      (#\} ,move-next-expr (values :end-object -1 -1))
		      (#\[ ,move-next-expr (values :start-array -1 -1))
		      (#\] ,move-next-expr (values :end-array -1 -1))
		      (#\Nul (values :eof -1 -1)))))
	   
	   (loop do (multiple-value-bind (evt start end) (next-event)
		      (declare (type symbol evt))
		      (declare (type fixnum start end))
		      (ecase evt
			(:true (json-stack-push stack :true))
			(:false (json-stack-push stack :false))
			(:null (json-stack-push stack :null))
			
			(:integer
			 (json-stack-push stack (parse-integer ,read-buffer-expr :start start :end (1+ end)))
			 ,reset-buffer-expr)
			
			(:float
			 (json-stack-push stack (read-from-string ,read-buffer-expr t #\Nul :start start :end (1+ end)))
			 ,reset-buffer-expr)
			
			(:string
			 (json-stack-push stack (subseq ,read-buffer-expr (1+ start) end))
			 ,reset-buffer-expr)
			
			(:escaped-string
			 (json-stack-push stack (json-string-unencode ,read-buffer-expr (1+ start) end))
			 ,reset-buffer-expr)
			
			(:start-object
			 (json-stack-push-object stack))
			
			(:end-key
			 (json-stack-validate-key stack))
			
			(:end-item
			 (json-stack-pop-item stack))
			
			(:end-object
			 (json-stack-pop-object stack))
			
			(:start-array
			 (json-stack-push-array stack))
			
			(:end-array
			 (json-stack-pop-array stack))
			
			(:eof
			 (if (zerop (json-stack-location stack))
			     (return (json-stack-pop stack))
			     (error "unexpected eof")))))))))))
