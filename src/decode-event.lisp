(in-package :simple-json-parser)

(deftype event (s)
  (member s '(:string :escaped-string :integer :float :true :false :null
	      :start-object :end-object :end-key :start-array :end-array :end-item :eof)))

(defstruct (parser (:conc-name ""))
  (read-buffer "" :type simple-string)
  (pos -1 :type fixnum)
  (eof nil :type boolean))

(defun next (my-parser)
  (declare (type parser my-parser))
  (if (< (incf (pos my-parser)) (length (read-buffer my-parser)))
      (aref (read-buffer my-parser) (pos my-parser))
      #\Nul))

(defun current (my-parser)
  (aref (read-buffer my-parser) (pos my-parser)))

(defun prev (my-parser)
  (declare (type parser my-parser))
  (aref (read-buffer my-parser) (1- (pos my-parser))))

(defun peek (my-parser)
  (declare (type parser my-parser))
  (if (< (1+ (pos my-parser)) (length (read-buffer my-parser)))
      (aref (read-buffer my-parser) (1+ pos))
      #\Nul))

(defun whitespace-p (c)
  (case (next my-parser)
    ((#\Space #\Tab #\Return #\Linefeed) t)
    (otherwise nil)))

(defun consume-number (start my-parser)
  (declare (type parser my-parser))

  (loop while (digit-char-p (peek my-parser))
	do (next my-parser))

  (when (whitespace-p (peek my-parser))
    (return-from consume-number (values :integer start (pos my-parser))))

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
  (declare (type parser my-parser))
  (loop with event-type = :string
	for c = (next) then (next)
	until (and (char= #\" c) (not (char= #\\ (prev my-parser))))
	do (if (char= #\\ c)
	       (case (peek my-parser)
		 ((#\" #\\ #\/ #\b #\f #\n #\r #\t #\u) (setf event-type :escaped-string))
		 (otherwise (json-error "Expecting escape sequence"))))
	finally (return (values event-type start (pos my-parser)))))

(defun consume-whitespace (my-parser)
  (declare (type parser my-parser))
  (loop while (whitespace-p my-parser)))

(defun consume-exact (my-parser expect)
  (declare (type parser my-parser))
  (declare (type simple-string expect))
  (loop for c across expect
	do (if (not (char= c (next my-parser)))
	       (error (format t "expected ~A" c)))
	finally (return (pos my-parser))))

;; :string :escaped-string :integer :float :true :false :null
;; :start-object :end-object :start-array :end-array :end-key :end-item :eof
(defun next-event (my-parser)
  (declare (type parser my-parser))
  (consume-whitespace)
  (let ((start (pos my-parser)))
    (case (next my-parser)
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

(defun decode-event (src)
  (let ((stack (make-array 1024))
	(stack-loc -1)
	(nesting -1)
	(my-parser (etypecase (src)
		     (simple-string (make-parser :read-buffer src)))))
    (declare (type parser my-parser))

    (labels ((pop-stack ()
	       (let ((ret (aref stack stack-loc)))
		 (decf stack-loc)
		 ret))
	     (push-stack (item)
	       (setf (aref stack (incf stack-loc)) item))
	     (pop-to-container ()
	       (if (typep (aref stack (1- stack-loc)) 'cons)
		   (let ((value (pop-stack))
			 (ary (cdr (aref stack stack-loc))))
		     (vector-push-extend value))
		   (let ((value (pop-stack))
			 (key (pop-stack))
			 (table (cdr (aref stack stack-loc))))
		     (setf (gethash key table) value))))
	     (pop-guard-empty ()
	       (let ((top (aref stack stack-loc)))
		 (if (not (and (typep top 'cons)
			       (= (car top) nesting)))
		     (pop-to-container)))))
      
      (loop do (multiple-value-bind (evt start end) (next-event my-parser)
		 (declare (type evt event))
		 (declare (type fixnum start end))
		 (etypecase (evt)
		   (:true (push-stack :true))
		   (:false (push-stack :false))
		   (:null (push-stack :null))
		   (:integer (push-stack (parse-integer (read-buffer my-parser) :start start :end (1+ end))))
		   (:float (push-stack (read-from-string (read-buffer my-parser) :start start :end (1+ end))))
		   ((:string :escaped-string) ;;TODO: need to escape string if necessary
		    (loop with s = (1+ start)
			  with ary = (make-string (- end s))
			  loop for i from s below end
			  do (setf (aref ary (- i s)) (aref (read-buffer my-parser) i))
			  finally (push-stack ary)))
		   
		   (:start-object
		    (setf (aref stack (incf stack-loc))
			  (cons (incf nesting) (make-hash-table :test #'equal))))
		   
		   (:end-key
		    (if (not (typep (aref stack stack-loc) 'string))
			(error "expecting string type as key")))

		   (:end-item
		    (pop-to-container))
		   
		   (:end-object
		    (pop-guard-empty)
		    (decf nesting))
		   
		   (:start-array
		    (setf (aref stack (incf stack-loc))
			  (cons (incf nesting) (make-array 0 :adjustable t :fill-pointer 0))))
		    
		   (:end-array
		    (pop-guard-empty)
		    (decf nesting))
		   
		   (:eof
		    (if (zerop stack-loc)
			(aref stack stack-loc)
			(error "unexpected eof"))))))))

    
  
  
