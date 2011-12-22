(in-package :blog)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defclass page-object ()
    ((display-name
      :accessor display-name
      :initarg :display-name)
     (page-symbol :accessor page-symbol
	     :initarg :symbol)
     (parent :accessor parent
	     :initarg :parent)
     (path :accessor path
	   :initarg :path)
     (rendercode :accessor rendercode
		 :initarg :rendercode)
     #|(renderlambda :accessor renderlambda
		   :initarg :renderlambda)|#
     (after-code :accessor after-code
		 :initarg :after-code)))


  #|(defclass topbar-object ()
    ((display-name
      :accessor display-name
      :initarg :display-name)
     (tb-symbol :accessor tb-symbol
	     :initarg :symbol)
     (parent :accessor parent
	     :initarg :parent)
     (path :accessor path
	   :initarg :path)
     (rendercode :accessor rendercode
		 :initarg :rendercode)
     #|(renderlambda :accessor renderlambda
		   :initarg :renderlambda)|#
     (after-code :accessor after-code
		 :initarg :after-code)))|#

  (defvar *page-object-hash* (make-hash-table))
)

(defun getpath (symbol)
    (path (gethash symbol *page-object-hash*)))

(ps:defpsmacro page-path (symbol)
  (render-url (getpath symbol)))

(ps:defpsmacro topbar-swap (symbol)
  `(progn (set-topbar (page-path ,symbol))
	  ,@(after-code (gethash symbol *page-object-hash*))))


#|(defun render-topbar (symbol stream &rest rest)
  (funcall (renderlambda (gethash symbol *topbar-hash*)) stream rest))|#

(defun render-url (pathspec) (reduce (lambda (x y) (format nil "~a/~a"  x y)) (cons "" pathspec)))

(defmacro render-page (page-symbol stream &rest rest)
  `(funcall ,(rendercode (gethash page-symbol *page-object-hash*)) ,stream ,@rest))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro topbar-expander (stream brand-def li-defs &rest rest)
    `(cl-who:with-html-output (,stream)
       (:div 
      :class "topbar"
      (:div 
       :class "fill"
       (:div
	:class "container"
	,brand-def
	(:ul 
	 :class "nav"
	 ,@li-defs)
	,@rest)))))

  (defmacro page-handler (tb-name)
    (let* ((stream (gensym))
	   (path (cdr (getpath tb-name))))
      
      `(defhandler (blog get (,@path)) (:|html|)
	 (reply (with-output-to-string (,stream)
		  (funcall ,(rendercode (gethash tb-name *page-object-hash*)) ,stream))))))

  
  (defmacro def-page ((symbol display-name (&rest pathspec) &key
			      (parent :self)
			      (stream (gensym))
			      (after-code nil))
		      (&rest args)
		      &body render-code)
    (let (#|(renderlambda (gensym))|#
	  (path (gensym))
	  (topbar (gensym))
	  (rendercode (gensym)))

      (let ((lambdacode `(lambda (,stream ,@args)
			     ,@render-code)))
	`(let* ((,rendercode ',lambdacode)
		#|(,renderlambda ,lambdacode)|#
		(,path ',(cons "blog" (cons "topbar" pathspec)))
		(,topbar (make-instance 'page-object
					:display-name ,display-name
					:symbol ',symbol
					:parent ',parent
					:path ,path
					:rendercode ,rendercode
					:after-code ,after-code
					#|:renderlambda ,renderlambda|# )))
	   (setf (gethash ',symbol *page-object-hash*) ,topbar)))))

  (defmacro def-topbar ((symbol display-name (&rest pathspec) &key (parent :self) (stream (gensym))
				(after-code nil))
			(&rest args)
			li-defs
			&rest rest)
    `(def-page (,symbol ,display-name (,@pathspec) :parent ,parent :stream ,stream :after-code ,after-code) (,@args)
       (topbar-expander ,stream
	(:a :class "brand" :href "/blog/index/"
	    ,display-name)
	(,@(if (eql parent :self) nil 
	       (list 
		`(:li (:a :href "#back"
			  :onclick (ps:ps-inline 
				    (topbar-swap ,parent))
			  "Back"))))
	   ,@li-defs)
	,@rest))))

(def-topbar (tb-logged-out "Multiblog" ("loggedout")) ()
  ((:li (:a :href "/blog/register" "Register"))
   (:li (:a :href "#" "About"))
   (:li (:a :href "/blog/main/jons" "Contact")))
  (:input :class "input-small" :id "user" :name "user" :type "text" :placeholder "Username")
  (:input :class "input-small" :id "password" :name "password" :type "password" :placeholder "Password")
  (:button :class "btn" 
	   :type "submit"
	   :onclick (ps:ps-inline (login
				   (ps:chain ($ "input#user")
					     (val))
				   (ps:chain ($ "input#password")
					     (val))))
	   "Sign In"))

(def-topbar (tb-logged-in "Main" ("loggedin")) ()
  ((:li (:a :href "#friends" :onclick (ps:ps-inline (topbar-swap tb-logged-in-friends)) "Friends"))
   (:li (:a :href "#followers" #|:onclick (ps:ps-inline (topbar-swap tb-logged-in-followers))|# "Followers"))
   (:li (:a :href "#post" :onclick (ps:ps-inline (topbar-swap tb-logged-in-post)) "Post"))
   (:li (:a :href "#settings" 
	    :onclick 
	    (ps:ps-inline (progn (topbar-swap tb-logged-in-settings)
				 (js-link "/blog/settings/" "div#blog" (lambda ()) 
					  (ps:create 
					   "session-id" (ps:chain ($ "input#session-id") (val))))))
	    "Settings"))
   (:li (:a :href "#chat" :onclick (ps:ps-inline (progn (topbar-swap tb-logged-in-chat))) "Chat"))
   (:li (:a :href "#logout" :onclick (ps:ps-inline (log-out)) "Logout")))
  (:input :class "input-small" :id "search" :name "search" :type "text" :placeholder "Search")
  (:button :class "btn" 
	   :type "submit"
	   :onclick (ps:ps-inline (do-search)) 
	   "Search"))

(def-topbar (tb-logged-in-basic "Main" ("loggedin" "basic")) ()
  ((:li (:a :href "#" :onclick (ps:ps-inline (redirect-user-page)) "My Page"))))

(def-topbar (tb-not-friend "Multiblog" ("view")) ()
  ((:li (:a :href "#addfriend" 
	    :onclick 
	    (ps:ps-inline (progn (topbar-swap tb-is-friend)
				 (add-friend)))
	    "Add Friend"))))

(def-topbar (tb-is-friend "Multiblog" ("view" "friend")) ()
  ((:li (:a :href "#defriend"
	    :onclick
	    (ps:ps-inline (progn (topbar-swap tb-not-friend)
				 (remove-friend)))
	    "Remove Friend"))))



(def-topbar (tb-logged-in-friends "Friends" ("friends") :parent tb-logged-in) ()
  ((:li (:a :href "#feed" 
	    :onclick
	    (ps:ps-inline (progn (topbar-swap tb-logged-in-friend-feed)
				 (js-link "/blog/friend-feed" "div#blog" (lambda ())
					  (ps:create :session-id (ps:chain ($ "input#session-id")
									   (val))
						     :start 0
						     :end 20))))
	    "Feed"))
   (:li (:a :href "#friends" 
	    :onclick
	    (ps:ps-inline		
	     ;(set-topbar "friends-list")
	     (js-link "/blog/friends" "div#blog" (lambda ())
		      (ps:create :session-id
				 (ps:chain ($ "input#session-id")
					   (val)))))
	    "List"))
   #|(:li (:a :href "#" "Add"))
   (:li (:a :href "#" "Remove"))|# )
  (:input :class "input-small" :id "search" :name "search" :type "text" :placeholder "Search")
  (:button :class "btn" 
	   :type "submit"
	   :onclick (ps:ps-inline (do-search)) 
	   "Search"))

(def-topbar (tb-logged-in-friend-feed  "Feed" ("friend" "feed") :parent tb-logged-in-friends) ()
  ((:li (:a :href "#older" 
	    :onclick
	    (ps:ps-inline (js-link "/blog/friend-feed" "div#blog" (lambda ())
				   (ps:create
				    :session-id (ps:chain ($ "input#session-id") (val))
				    :start (+ (* 1 (ps:chain ($ "input#chat-start") (val))) 20)
				    :end (+ (* 1 (ps:chain ($ "input#chat-end") (val))) 20))))
	    "Older"))
   (:li (:a :href "#newer" 
	    :onclick
	    (ps:ps-inline
	     (js-link "/blog/friend-feed" "div#blog" (lambda ())
		      (ps:create
		       :session-id (ps:chain ($ "input#session-id") (val))
		       :start (- (* 1 (ps:chain ($ "input#chat-start") (val))) 20)
		       :end (- (* 1 (ps:chain ($ "input#chat-end") (val))) 20))
		      )) "Newer"))))

(def-topbar (tb-logged-in-chat "Chat" ("chat") :parent tb-logged-in) ()
  ((:li (:a :href "#subs" :onclick (ps:ps-inline 
				    (js-link "/blog/chat/subs" "div#blog" (lambda ())
					     (ps:create "session-id" (ps:chain ($ "input#session-id") (val)))))
	    "Subscribed"))
   (:li (:a :href "#new"
	    :onclick (ps:ps-inline (js-link "/blog/chat/create/" "div#blog")) "New")))
  (:input :class "input-small" :id "search" :name "search" :type "text" :placeholder "Search")
  (:button :class "btn" 
	   :type "submit"
	   :onclick (ps:ps-inline (do-search)) 
	   "Search"))

(def-topbar (tb-logged-in-chat-manager "Manager" ("chat" "manager") :parent tb-logged-in-chat) ()
  ((:li (:a :href "#new" "New"))
   (:li (:a :href "#edit" "Edit"))
   (:li (:a :href "#remove" "Remove"))))

(def-topbar (tb-logged-in-chat-history "History" ("chat" "history") :parent tb-logged-in-chat) ()
  ((:li (:a :href "#previous"
	    :onclick 
	    (ps:ps-inline (chat-history (+ (* 1 (ps:chain ($ "input#ch-start") (val))) 20)
					(+ (* 1 (ps:chain ($ "input#ch-end") (val))) 20))) "Previous"))
   (:li (:a :href "#next"
	    :onclick
	    (ps:ps-inline (chat-history (- (* 1 (ps:chain ($ "input#ch-start") (val))) 20)
					(- (* 1 (ps:chain ($ "input#ch-end") (val))) 20))) "Next"))))

(def-topbar (tb-logged-in-post "Post" ("post") :parent tb-logged-in) ()
  ((:li (:a :href "#New" :onclick (ps:ps-inline (js-link "/blog/post/new" "div#blog")) "Add"))
   (:li (:a :href "#Edit"
	    :onclick 
	    (ps:ps-inline 
	     (js-link "/blog/post/edit" "div#blog" (lambda ())
		      (ps:create :session-id (ps:chain ($ "input#session-id") (val))))) "Update")))
  (:input :class "input-small" :id "search" :name "search" :type "text" :placeholder "Search")
  (:button :class "btn" 
	   :type "submit"
	   :onclick (ps:ps-inline (do-search)) 
	   "Search"))

(def-topbar (tb-logged-in-settings "Settings" ("settings") :parent tb-logged-in) ()
  ((:li (:a :href "#" :onclick 
	    (ps:ps-inline 
	     (progn (topbar-swap tb-logged-in-settings)
		    (js-link "/blog/settings/" "div#blog" (lambda ()) 
			     (ps:create 
			      "session-id" (ps:chain ($ "input#session-id") (val)))))) "Settings"))
   (:li (:a :href "#" "Picture"))
   (:li (:a :href "#" "Bio"))
   (:li (:a :href "#" "Style")))
  (:input :class "input-small" :id "search" :name "search" :type "text" :placeholder "Search")
  (:button :class "btn" 
	   :type "submit"
	   :onclick (ps:ps-inline (do-search)) 
	   "Search"))