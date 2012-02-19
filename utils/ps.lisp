(in-package :blog)

(ps:defpsmacro session-id ()
  `(ps:chain ($ "input#session-id") (val)))

(ps:defpsmacro embedlify ()
  `(ps:chain ($ "div#blog a") 
	     (embedly 
	      (ps:create "maxWidth" 450
			 "method" "after"
			 "wmode" "transparent" 
			 "allowscripts" true
			 "success" (lambda (oembed dict)
				     (let ((elem ($ (ps:chain dict node)))
					   (code (ps:chain oembed code)))
				       (ps:chain elem (after code)))
				     (let ((scrollbar ($ "#blogscrollbar")))
				       (ps:chain scrollbar (tinyscrollbar))
				       (ps:chain scrollbar (tinyscrollbar_update))))
			 ))))

(ps:defpsmacro val-of (jquery)
  `(ps:chain ($ ,jquery) (val)))

(ps:defpsmacro set-val-of (jquery value)
  `(ps:chain ($ ,jquery) (val ,value)))

(ps:defpsmacro session-obj (&rest rest)
  `(ps:create "session-id" (session-id)
	      ,@rest))

(defvar *div-link-effects* (gensym "div-link-effects"))

(ps:defpsmacro js-link (link div-id &optional afterfn object)
  (let ((data (gensym))
	(effects (gensym))
	(effect (gensym)))
    `($.get ,link
	    ,(if object object `(ps:create))
	    (lambda (,data)
	      (ps:chain ($ ,div-id) 
			(html ,data))
	      (let ((,effects (ps:getprop ,*div-link-effects* ,div-id)))
		(dolist (,effect ,effects)
		  (funcall ,effect ,div-id ,data)))
	      ,@(if afterfn
		    `((,afterfn))
		    ())))))

(ps:defpsmacro def-link-effect (div-id args &body body)
  (let ((properties (gensym)))
  `(let ((,properties (ps:getprop ,*div-link-effects* ,div-id)))
     (if ,properties
	 (ps:chain ,properties (push (lambda (,@args) ,@body)))
	 (setf (ps:getprop ,*div-link-effects* ,div-id) (array (lambda (,@args) ,@body)))))))

(ps:defpsmacro defpostfn (name path 
				 (args1 &body body1) 
				 (args2 &body body2))
    (let ((strings (mapcar #'(lambda (symbol) (string-downcase (symbol-name symbol))) path)))
      (let ((path-name (reduce (lambda (name1 name2)
				 (format nil "~a~a/" name1 name2)) 
			       (cons (format nil "/~a/" (car strings))
				     (cdr strings))))
	    (post-result (gensym)))
	`(defun ,name (,@args1)
	   (let ((,post-result (progn ,@body1)))
	     ($.post 
	      ,path-name
	      ,post-result
	      (lambda (,@args2) ,@body2)))))))

(defun clickable-li (stream name &rest rest)
    (cl-who:with-html-output (stream)
      (:li :onclick (ps:ps-inline* `(js-link ,@rest))
	   (cl-who:str name))))

(defun named-link (stream name &rest rest)
  (cl-who:with-html-output (stream)
    (:a :onclick  
	(ps:ps-inline* `(js-link ,@rest))
	(cl-who:str name))))

(ps:defpsmacro popup-link (url object &rest rest)
  (let ((urlg (gensym))
	(parametersg (gensym))
	(tailg (gensym))
	(keyg (gensym))
	(valueg (gensym))
	(url-with-parameters (gensym)))
    `(let ((,urlg ,url)
	   (,parametersg ,object)
	   (,tailg (ps:array)))
       ($.each ,parametersg (lambda (,keyg ,valueg)  
			      (ps:chain ,tailg (push (concatenate 'string ,keyg  "=" (encode-u-r-i-component ,valueg))))))
       (let ((,url-with-parameters (concatenate 'string ,urlg "?" (ps:chain ,tailg (join "&")))))
	 (ps:chain window (open ,url-with-parameters ,@rest))))))

(defun named-popup-link (stream name &rest rest)
  (cl-who:with-html-output (stream)
    (:a :onclick (ps:ps-inline* `(popup-link ,@rest))
	(cl-who:str name))))

(defun clickable-popup-link (stream name &rest rest)
  (cl-who:with-html-output (stream)
    (:li :onclick (ps:ps-inline* `(popup-link ,@rest))
	 (cl-who:str name))))