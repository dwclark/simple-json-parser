(in-package :simple-json-parser)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(declaim (inline str-parser-read-buffer str-parser-pos str-parser-move-next str-parser-current str-parser-next str-parser-reset
		 whitespace-p consume-whitespace consume-exact consume-string consume-number next-event
		 json-stack-push json-stack-at json-stack-pop json-stack-push-array
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

(defun str-parser-reset (parser)
  (declare (ignore parser)))

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

(defun consume-number (my-parser)
  (declare (type str-parser my-parser))
  (let ((start (the fixnum (str-parser-pos my-parser)))
	(current (the character (str-parser-current my-parser))))

    (when (or (char= #\+ current) (char= #\- current))
      (setf current (str-parser-next my-parser)))

    (when (not (digit-char-p current))
      (error "bare +/- sign found"))
      
    (loop while (digit-char-p (setf current (str-parser-next my-parser))))
    
    (when (or (whitespace-p current) (char= current #\,) (char= current #\]) (char= current #\}) (char= current #\Nul))
      (return-from consume-number (values :integer start (1- (str-parser-pos my-parser)))))
    
    (when (char= #\. current)
      (setf current (str-parser-next my-parser))
      (if (not (digit-char-p current))
	  (error "expected digits")))
    
    (loop while (digit-char-p (setf current (str-parser-next my-parser))))
    
    (when (or (whitespace-p current) (char= current #\,) (char= current #\]) (char= current #\}) (char= current #\Nul))
      (return-from consume-number (values :float start (1- (str-parser-pos my-parser)))))

    (when (not (char-equal #\e current))
      (error "expected 'e' character"))

    (setf current (str-parser-next my-parser))
    (when (or (char= #\+ current) (char= #\- current))
      (setf current (str-parser-next my-parser))
      (if (not (digit-char-p current))
	  (error "expected digits")))

    (loop while (digit-char-p (setf current (str-parser-next my-parser))))
    
    (if (or (whitespace-p current) (char= current #\,) (char= current #\]) (char= current #\}) (char= current #\Nul))
	(values :float start (1- (str-parser-pos my-parser)))
	(error "expected digits"))))

(defun consume-string (my-parser)
  (declare (type str-parser my-parser))
  (loop with start of-type fixnum = (str-parser-pos my-parser)
	with end of-type fixnum = 0
	with event-type = :string
	with prev of-type character = #\Nul
	for c of-type character = (str-parser-next my-parser) then (str-parser-next my-parser)
	until (and (char= #\" c) (not (char= #\\ prev)))
	do (when (char= #\\ c)
	     (setf c (str-parser-next my-parser))
	     (case c
	       ((#\" #\\ #\/ #\b #\f #\n #\r #\t #\u) (setf event-type :escaped-string))
	       (otherwise (error "Expecting escape sequence"))))
	   (setf prev c)
	finally (setf end (str-parser-pos my-parser))
		(str-parser-move-next my-parser)
		(return (values event-type start end))))

(defun consume-whitespace (my-parser)
  (declare (type str-parser my-parser))
  (loop for c of-type character = (str-parser-current my-parser) then (str-parser-next my-parser)
	while (whitespace-p c)
	finally (return c)))

(defun consume-exact (my-parser expect)
  (declare (type str-parser my-parser))
  (declare (type simple-string expect))
  (loop for c across expect
	do (if (not (char= c (str-parser-next my-parser)))
	       (error (format t "expected ~A" c)))
	finally (str-parser-move-next my-parser)))

(defun next-event (my-parser)
  (declare (type str-parser my-parser))
  (ecase (consume-whitespace my-parser)
    ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (consume-number my-parser))
    (#\" (consume-string my-parser))
    (#\t
     (consume-exact my-parser "rue")
     (values :true -1 -1))
    (#\f
     (consume-exact my-parser "alse")
     (values :false -1 -1))
    (#\n
     (consume-exact my-parser "ull")
     (values :null -1 -1))
    
    (#\, (str-parser-move-next my-parser) (values :end-item -1 -1))
    (#\: (str-parser-move-next my-parser) (values :end-key -1 -1))      
    (#\{ (str-parser-move-next my-parser) (values :start-object -1 -1))
    (#\} (str-parser-move-next my-parser) (values :end-object -1 -1))
    (#\[ (str-parser-move-next my-parser) (values :start-array -1 -1))
    (#\] (str-parser-move-next my-parser) (values :end-array -1 -1))
    (#\Nul (values :eof -1 -1))))

(defun decode-event (src)
  (let* ((stack (make-json-stack))
	 (*read-default-float-format* 'double-float)
	 (my-parser (etypecase src
		      (simple-string (make-str-parser :read-buffer src :pos 0)))))
    
    (loop do (multiple-value-bind (evt start end) (next-event my-parser)
	       (declare (type symbol evt))
	       (declare (type fixnum start end))
	       (ecase evt
		 (:true (json-stack-push stack :true))
		 (:false (json-stack-push stack :false))
		 (:null (json-stack-push stack :null))

		 (:integer
		  (json-stack-push stack (parse-integer (str-parser-read-buffer my-parser) :start start :end (1+ end)))
		  (str-parser-reset my-parser))
		 
		 (:float
		  (json-stack-push stack (read-from-string (str-parser-read-buffer my-parser) t #\Nul :start start :end (1+ end)))
		  (str-parser-reset my-parser))
		 
		 (:string
		  (json-stack-push stack (subseq (str-parser-read-buffer my-parser) (1+ start) end))
		  (str-parser-reset my-parser))

		 (:escaped-string
		  (json-stack-push stack (json-string-unencode (str-parser-read-buffer my-parser) (1+ start) end))
		  (str-parser-reset my-parser))
		 
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
		      (error "unexpected eof"))))))))
