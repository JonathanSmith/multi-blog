(in-package :blog)

(defvar *salt*)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun uuid-string (&optional (uuid (uuid:make-v4-uuid)))
    (with-open-stream (s (make-string-output-stream))
      (uuid:print-bytes s uuid)
      (get-output-stream-string s)))

  (defun obfuscate-password (password)
  (let* ((salted (concatenate 'string *salt* password)))
    (map 'string #'code-char (md5::MD5SUM-SEQUENCE salted))))

  (defmacro destructure-props (indicator-plist list &body body)
    (let ((gkey (gensym))
	  (glist (gensym))
	  (gval (gensym))
	  (grest (gensym)))
      `(let ((,glist ,list))
	 (let (,@(mapcar #'first indicator-plist))
	   (do* ((,gkey (car ,glist) (car ,grest))
		 (,gval (cadr ,glist) (cadr ,grest))
		 (,grest (cddr ,glist) (cddr ,grest)))
		((null ,gkey))
	     (cond ,@(mapcar (lambda (pair) 
			       `((string= ,(second pair) ,gkey)
				 (setf ,(first pair) ,gval)))
			     indicator-plist)))
	   ,@body))))

  (defmacro bind-query (query-bind pairlist &body body)
    (let ((q (or (first query-bind) (gensym "q"))))
      `(let* ((,q (parse-query lisp-on-yaws::*query*)))
	 (let ,(mapcar (lambda (pair)
			 (let ((symbol (first pair))
			       (string (second pair)))
			   `(,symbol (second (assoc ,string ,q :test #'string=))))) pairlist)
	   ,@body)))))


(defun has-textp (string)
  (and (> (length string) 0) (some #'alphanumericp string)))

(defun safe-parse-int (string &optional (radix 10))
  (when (and string (every #'(lambda (char) (digit-char-p char radix)) string))
    (parse-integer string)))

(defun timestamp ()
  (multiple-value-bind (second minute hour date month year)  
      (decode-universal-time (get-universal-time))
    (format nil "~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun getprop (list indicator &optional (default nil))
  (do* ((key (car list) (car rest))
	(val (cadr list) (cadr rest))
	(rest (cddr list) (cddr rest)))
       ((or (string-equal key indicator) (null key)) (or val default))))