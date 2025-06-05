(in-package :simple-json-parser)

(declaim (optimize (speed 0) (debug 3)))

(defgeneric current (parser))
(defgeneric next (parser))
(defgeneric prev (parser))
(defgeneric peek (parser))

(defclass simple-string-parser ()
  ((read-buffer :initarg :read-buffer :initform (error "must supply read buffer") :reader read-buffer)
   (pos :initarg :pos :initform -1 :reader pos)))

(defmethod next ((parser simple-string-parser))
  (with-slots (read-buffer pos) parser
    (if (< (incf pos) (length read-buffer))
	(aref read-buffer pos)
	#\Nul)))

(defmethod current ((parser simple-string-parser))
  (with-slots (read-buffer pos) parser
    (if (< pos (length read-buffer))
	(aref read-buffer pos)
	#\Nul)))

(defmethod prev ((parser simple-string-parser))
  (with-slots (read-buffer pos) parser
    (aref read-buffer (1- pos))))

(defmethod peek ((parser simple-string-parser))
  (with-slots (read-buffer pos) parser
    (if (< (1+ pos) (length read-buffer))
	(aref read-buffer (1+ pos))
	#\Nul)))

(defun whitespace-p (c)
  (case c
    ((#\Space #\Tab #\Return #\Linefeed) t)
    (otherwise nil)))

(defun consume-number (start my-parser)
  (loop while (digit-char-p (peek my-parser))
	do (next my-parser))

  (let ((c (peek my-parser)))
    (when (or (whitespace-p c) (char= c #\Nul))
      (return-from consume-number (values :integer start (pos my-parser)))))

  (when (char= #\. (peek my-parser))
    (next my-parser)
    (if (digit-char-p (peek my-parser))
	(loop while (digit-char-p (peek my-parser))
	      do (next my-parser))
	(error "expected digits")))
  
  (when (char-equal #\e (peek my-parser))
    (next my-parser)
    (when (or (char= #\+ (peek my-parser))
	      (char= #\- (peek my-parser)))
      (next my-parser))
    (if (digit-char-p (peek my-parser))
	(loop while (digit-char-p (peek my-parser))
	      do (next my-parser))
	(error "expected digits")))

  (values :float start (pos my-parser)))

(defun consume-string (start my-parser)
  (loop with event-type = :string
	for c = (next my-parser) then (next my-parser)
	until (and (char= #\" c) (not (char= #\\ (prev my-parser))))
	do (if (char= #\\ c)
	       (case (peek my-parser)
		 ((#\" #\\ #\/ #\b #\f #\n #\r #\t #\u) (setf event-type :escaped-string))
		 (otherwise (json-error "Expecting escape sequence"))))
	finally (return (values event-type start (pos my-parser)))))

(defun consume-whitespace (my-parser)
  (loop while (whitespace-p (next my-parser))))

(defun consume-exact (my-parser expect)
  (loop for c across expect
	do (if (char= c (peek my-parser))
	       (next my-parser)
	       (error (format t "expected ~A" c)))
	finally (return (pos my-parser))))

;; :string :escaped-string :integer :float :true :false :null
;; :start-object :end-object :start-array :end-array :end-key :end-item :eof
(defun next-event (my-parser)
  (consume-whitespace my-parser)
  (let ((start (pos my-parser)))
    (format t "next-event: start: ~A, current: ~A~%" start (current my-parser))
    (case (current my-parser)
      (#\t (values :true start (consume-exact my-parser "rue")))
      (#\f (values :false start (consume-exact my-parser "alse")))
      (#\n (values :null start (consume-exact my-parser "ull")))
      (#\, (values :end-item start start))
      (#\: (values :end-key start start))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (consume-number start my-parser))
      (#\" (consume-string start my-parser))
      (#\{ (values :start-object start start))
      (#\} (values :end-object start start))
      (#\[ (values :start-array start start))
      (#\] (values :end-array start start))
      (#\Nul (values :eof -1 -1)))))

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

(defun decode-event (src)
  (let ((stack (make-json-stack))
	(my-parser (etypecase src
		     (simple-string (make-instance 'simple-string-parser :read-buffer src)))))
    
    (loop do (multiple-value-bind (evt start end) (next-event my-parser)
	       (format t "evt: ~A, start: ~A, end: ~A~%" evt start end)
	       (ecase evt
		 (:true (json-stack-push stack :true))
		 (:false (json-stack-push stack :false))
		 (:null (json-stack-push stack :null))
		 (:integer
		  (json-stack-push stack (parse-integer (read-buffer my-parser) :start start :end (1+ end))))
		 
		 (:float
		  (let ((*read-default-float-format* 'double-float))
		    (json-stack-push stack (read-from-string (read-buffer my-parser) t #\Nul :start start :end (1+ end)))))
		 
		 ((:string :escaped-string) ;;TODO: need to escape string if necessary
		  (json-stack-push stack (subseq (read-buffer my-parser) (1+ start) end)))
		 
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
