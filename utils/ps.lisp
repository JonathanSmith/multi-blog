(in-package :blog)

(ps:defpsmacro js-link (link div-id &optional afterfn object)
    (let ((data (gensym)))
      `($.get ,link
	      ,(if object object `(ps:create))
	      (lambda (,data)
		(console.log ,data)
		(ps:chain ($ ,div-id) 
			  (html ,data))
		,@(if afterfn
		      `((,afterfn))
		      ())))))

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
    (:a :href "#" :onclick  
	(ps:ps-inline* `(js-link ,@rest))
	(cl-who:str name))))