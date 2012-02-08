(in-package "BLOG")

(defvar *site-cookie-name*)
(defvar *domain-root* "0.0.0.0:8080")
(defvar *reply-chat-thread*)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro reply-status (status &rest plist)
    (let (values-list
	  keys-list
	  gsyms)
      (let ((odd t))
	(map nil
	     #'(lambda (val) 
		 (if (stringp val)
		     (if odd
			 (push val keys-list)
			 (push val values-list))
		     (let ((gsym (gensym)))
		       (push (list gsym val) gsyms)
		       (if odd
			   (push gsym keys-list)
			   (push gsym values-list))))
		 (setf odd (not odd))) plist))
      (let ((body
	     `(reply (json:encode-json-to-string (list (cons "status" ,status)
						       ,@(mapcar (lambda (key val) `(cons ,key ,val)) 
								 keys-list
								 values-list))))))
	(if gsyms
	    `(let ,gsyms
	       ,body)
	    body)))))

(defhandler (blog get ("viewpost" postid)) (:|html|)
  (bind-query () ((session-id "session-id"))
    (if postid
	(reply (cl-who:with-html-output-to-string (var)
		 (cl-who:str (generate-post-from-db postid))
		 (when (check-login session-id)
		   (named-link var "Reply" 
			       "/blog/post/new/"
			       "div#blog"
			       '(lambda ()) `(session-obj "reply-to" ,postid)))))
	(reply ""))))

(defhandler (blog get ("index" author)) (:|html|)
  (bind-query ()
      ((start "start") (end "end"))
    (setf start (safe-parse-int start))
    (setf end (safe-parse-int end))
    (reply (cond ((and start end)
		  (generate-index author :start start :end end))
		 (start
		  (generate-index author :start start))
		 (end (generate-index author :end end))
		 (t (generate-index author))))))
  

(defhandler (blog get ("last_post" author)) (:|content| "application/json")
    (reply (most-recent-post author)))

(defhandler (blog get ("jslib")) (:|content| "application/javascript")
  (reply 
   (concatenate 'string
		(ps:ps* `(defun init-login ()
			   (let ((session-id (get-cookie ,*site-cookie-name*)))
			     ($.post "/blog/re-auth/" (ps:create "session-id" session-id)
				     (lambda (data textstatus qxhr)
				       (if (equal (ps:getprop data 'status) "success")
					   (progn
					     (ps:chain ($ "input#session-id") (val session-id))
					     (after-login (ps:getprop data 'user))))))))
			`(defun log-out ()
			   ($.post "/blog/logout/" (session-obj))
			   (ps:chain ($ "input#session-id") "")
			   (set-cookie ,*site-cookie-name* "" 0)
			   (ps:chain ($ "div#chat") (html ""))
			   (init-login)
			   (set-topbar (page-path tb-logged-out))))
		(ps:ps
		  (defvar *logged-in-user* nil)
		  (defpostfn login (blog login)
		    ((user password) 
		     (ps:create "user" user "password" password))
		    ((data texstatus qxhr)
		     (let ((status (ps:getprop data 'status)))
		       (when (eql status "success")
			 (let ((expires (ps:getprop data 'expires))
			       (cookie-id (ps:getprop data 'cookie-id))
			       (session-id (ps:getprop data 'session-id)))
			   (ps:chain ($ "input#session-id") (val session-id))
			   (set-cookie cookie-id session-id expires)
			   (after-login user))))))
		  (defun set-topbar (path)
		    ($.get path
			   (session-obj)
			   (lambda (topbarhtml)
			     (ps:chain
			      ($ "div#topbar")
			      (html  topbarhtml)))))
		  (defun set-cookie (c-name value exdays)
		    (let ((exdate (ps:new (-date))))
		      (ps:chain exdate (set-date (+ (ps:chain exdate (get-Date)) exdays)))
		      (let ((c-val (concatenate 
				    'string
				    (escape value)
				    (if (not exdays)
					""
					(concatenate 
					 'string
					 #.(format nil "; expires=")
					 (ps:chain exdate (to-u-t-c-string))
					 ))
				    "; path=/")))
			(setf (ps:chain document cookie)
			      (concatenate 'string c-name "=" c-val)))))
		  
		  (defun get-cookie (cname)
		    (let ((arr-cookies  (ps:chain document cookie (split ";"))))
		      (let (eqlidx x y r) 
			(do* ((i 0 (+ i 1))
			      (current (ps:getprop arr-cookies i) (ps:getprop arr-cookies i)))
			     ((or (equal r cname) (>= i (ps:chain arr-cookies length)))
			      (if (equal r cname)  y  y))
			  (setf eqlidx (ps:chain current (index-of "=")))
			  (setf x (ps:chain current (substr 0 eqlidx)))
			  (setf y  (ps:chain current (substr (+ eqlidx 1))))
			  (setf r (ps:chain x (replace (ps:regex "/^\s|\s|$/g") "")))))))

		  (defun after-login (user)
		    (topbar-swap tb-logged-in-basic)
		    (setf *logged-in-user* user))
		  (defun redirect-user-page ()
		    (setf window.location (concatenate 'string "/blog/main/" *logged-in-user*)))))))

(defhandler (blog get ("jslib" author)) (:|content| "application/javascript")
  (setf author (string-downcase author))
  (let ((chat-id (hgetredis author "default-chat" *settings-ns*)))
    (reply 
     (concatenate 'string  
		  (let ((most-recent-post (most-recent-post author)))
		    (ps:ps* 
		     `(defvar author ,author)
		     `(defvar *most-recent-post* ,most-recent-post) 
		     `(defun get-init-post ()
			(js-link (concatenate 'string "/blog/viewpost/"
					      *most-recent-post*)
				 "div#blog" (lambda ()) (session-obj))
			(update-index)
			;,@(if chat-id `((js-link ,(format nil "/blog/chat/i/~a" chat-id) "div#chat" (lambda ()) (session-obj))))
			)

		     `(defun init-login ()
			(let ((session-id (get-cookie ,*site-cookie-name*)))
			  ($.post "/blog/re-auth/" (ps:create "session-id" session-id)
				  (lambda (data textstatus qxhr)
				    (if (equal (ps:getprop data 'status) "success")
					(progn
					  (ps:chain ($ "input#session-id") (val session-id))
					  (after-login (ps:getprop data 'user))
					  (update-index)))
				    ,@(if chat-id
						`((js-link ,(format nil "/blog/chat/i/~a" chat-id) "div#chat" (lambda ()) (session-obj)))
						nil)))))
		     `(defun log-out ()
			(set-cookie ,*site-cookie-name* "" 0)
			(ps:chain ($ "div#chat") (html ""))
			(init-login)
			(get-init-post)
			(set-topbar (page-path tb-logged-out)))

		     `(defun update-index ()
			($.get ,(format nil "/blog/index/~a" author)
			       (ps:create :start 0 :end 20)
			       (lambda (data)
				 (ps:chain 
				  ($ "div#index")
				  (html data)))))

		    #| `(defun check-last-post ()
			($.get ,(format nil "/blog/last_post/~a" author)  
			       (ps:create)
			       (lambda (server-id)
				 (let ((this-id (val-of "input#latest")))
				   (unless (equal this-id server-id)
				     (update-index)
				     (ps:chain ($ "input#latest") (val server-id)))))
			       "json"))|#
))
		  (ps:ps
		    (defvar *chat-id* false)
		    (defvar *chat-callback*)
		    (defvar *show-timestamps* false)
		    (defun create-chat ()
		      ($.post "/blog/chat/create/" 
			      (session-obj
			       "chat-name" (val-of "input#chat-name"))
			      (lambda (data textstatus qxhr)
				(if (equal (ps:getprop data 'status) "success")
				    (let ((chat-link (concatenate 'string "/blog/chat/i/" (ps:getprop data "chat-id"))))
				      (js-link chat-link "div#chat"))
				    (alert "failure")))))
		    (defun manage-timestamps ()
		      (if *show-timestamps*
			  (ps:chain 
			   ($ "span.timestamp") (show))
			  (ps:chain ($ "span.timestamp")
				    (hide))))

		    (defun show-timestamps ()
		      (setf *show-timestamps* true)
		      (manage-timestamps))

		    (defun hide-timestamps ()
		      (setf *show-timestamps* false)
		      (manage-timestamps))
		    
		    (defun after-login (user)
		      (if (equal user author)
			  (progn (topbar-swap tb-logged-in)
				 (ps:chain ($ "div#index") (html ""))
				 (js-link "/blog/friend-feed" "div#blog" (lambda ()) (session-obj
										      "start" 0
										      "end" 20)))
			  (progn (set-topbar "/blog/topbar/other")
				 (js-link (concatenate 'string "/blog/viewpost/"
						       *most-recent-post*)
					  "div#blog" (lambda ()) (session-obj)))))
		    
		    (defun set-topbar (path)
		      ($.get path
			     (session-obj
			      "author" author)
			     (lambda (topbarhtml)
			       (ps:chain
				($ "div#topbar")
				(html  topbarhtml)))))
		    (defun set-cookie (c-name value exdays)
		      (let ((exdate (ps:new (-date))))
			(ps:chain exdate (set-date (+ (ps:chain exdate (get-Date)) exdays)))
			(let ((c-val (concatenate 
				      'string
				      (escape value)
				      (if (not exdays)
					  ""
					  (concatenate 
					   'string
					   #.(format nil "; expires=")
					   (ps:chain exdate (to-u-t-c-string))
					   ))
				      "; path=/")))
			  (setf (ps:chain document cookie)
				(concatenate 'string c-name "=" c-val)))))
		  

		    (defun get-cookie (cname)
		      (let ((arr-cookies  (ps:chain document cookie (split ";"))))
			(let (eqlidx x y r) 
			  (do* ((i 0 (+ i 1))
				(current (ps:getprop arr-cookies i) (ps:getprop arr-cookies i)))
			       ((or (equal r cname) (>= i (ps:chain arr-cookies length)))
				(if (equal r cname)  y  y))
			    (setf eqlidx (ps:chain current (index-of "=")))
			    (setf x (ps:chain current (substr 0 eqlidx)))
			    (setf y  (ps:chain current (substr (+ eqlidx 1))))
			    (setf r (ps:chain x (replace (ps:regex "/^\s|\s|$/g") "")))))))
				
		    #|(defun poll-index ()
		      (ps:var timer (set-interval "checkLastPost()" 30000)))|#

		    (defpostfn make-post (blog post new)
		      ((title text &optional (reply-to false))
		       (if reply-to
			   (session-obj
			    "title" title
			    "post" text
			    "reply-to" reply-to)
			   (session-obj
			    "title" title
			    "post" text)))
		      ((data textstatus qxhr)
		       (let ((status (ps:getprop data 'status)))
			 (if (equal status "success")
			     (let* ((most-recent-post (ps:getprop data 'post-id))
				    (user (ps:getprop data 'user))
				    (posts-link (concatenate 'string
							     "/blog/viewpost/" most-recent-post))
				    (indexes-link (concatenate 'string "/blog/index/" user)))
			       (js-link posts-link "div#blog" (lambda ()) (session-obj))
					;(js-link "/blog/chat/" "div#chat" chat-loop-init)
			     
			       (js-link indexes-link "div#index")
			       (ps:chain ($ "div#notify") (html (concatenate 'string "Post Success!"))))

			     (ps:chain ($ "div#notify") 
				       (html (concatenate 'string "Post Failure!")))))))

		    (defpostfn update-post (blog post edit)
		      ((title text id)
		       (session-obj
			"title" title
			"post" text
			"post-id" id))
		      ((data textstatus qxhr)
		       (let ((status (ps:getprop data 'status)))
			 (if (equal status "success")
			     (let* ((most-recent-post (ps:getprop data 'post-id))
				    (user (ps:getprop data 'user))
				    (posts-link (concatenate 'string  "/blog/viewpost/" most-recent-post))
				    (indexes-link (concatenate 'string "/blog/index/" user)))
			       (js-link posts-link "div#blog" (lambda ()) (session-obj))
					;(js-link "/blog/chat/" "div#chat" chat-loop-init)
			       (js-link indexes-link "div#index")
			       (ps:chain ($ "div#notify") (html (concatenate 'string "Post Success!"))))

			     (ps:chain ($ "div#notify") 
				       (html (concatenate 'string "Post Failure!")))))))

		    (defpostfn update-settings (blog settings)
		      ((object) object)
		      ((data texstatus qxhr)
		       (let ((status (ps:getprop data 'status)))
			 (when (eql status "success")
			   (progn
			     (ps:chain ($ "h2#title") (html (ps:getprop data "title")))
			     (ps:chain ($ "p#subtitle") (html (ps:getprop data "subtitle"))))))))

		    (defpostfn login (blog login)
		      ((user password) 
		       (ps:create "user" user "password" password))
		      ((data texstatus qxhr)
		       (let ((status (ps:getprop data 'status)))
			 (when (eql status "success")
			   (let ((expires (ps:getprop data 'expires))
				 (cookie-id (ps:getprop data 'cookie-id))
				 (session-id (ps:getprop data 'session-id)))
			     (ps:chain ($ "input#session-id") (val session-id))
			     (set-cookie cookie-id session-id expires)
			     (after-login user))))))

		    (defpostfn add-friend (blog friends json add)
		      (()
		       (session-obj
			"friends" author))
		      ((data texstatus qxhr)))
		  
		    (defpostfn remove-friend (blog friends json remove)
		      (()
		       (session-obj
			"friends" author))
		      ((data texstatus qxhr)))


		    (defpostfn delete-post (blog post remove)
		      ((post-id)
		       (session-obj
			"post-id" post-id))
		      ((data texstatus qxhr)
		       (let ((status (ps:getprop data 'status)))
			 (when (eql status "success")
			   (let ((post-id (ps:getprop data 'post-id)))
			     (ps:chain ($ "div#blog") (html (concatenate 'string "Post " post-id " Deleted!"))))))))

		    (defun chat-loop-init ()
		      ;;need a better way to do this.
		      ;;so that I only need to maintain 1 open connection
		      ;;per chat.
		      (when *chat-callback*
			(ps:chain *chat-callback* (abort)))
		      ($.get (concatenate 'string "/blog/chat/instant/" *chat-id*)
			     (session-obj)
			     (lambda (data)
			       (ps:chain ($ "div#chatwindow") (html data))
			       (manage-timestamps)
			       (chat-loop))))

		    (defun chat-loop ()
		      (setf *chat-callback* ($.get (concatenate 'string "/blog/chat/wait/" *chat-id*)
						   (session-obj)
						   (lambda (data)
						     (ps:chain ($ "div#chatwindow") (html data))
						     (manage-timestamps)
						     (chat-loop)))))
			       
		    (defun chat-history (chat-id start end)
		      ($.get (concatenate 'string "/blog/chat/history/" chat-id)
			     (session-obj
			      :start start
			      :end end)
			     (lambda (data)
			       (let ((status (ps:getprop data 'status)))
				 (if (eql status "success")
				     (let ((result (ps:getprop data 'result)))
				       (ps:chain ($ "div#blog")
						 (html result))
				       (manage-timestamps)
				       ))))))

		    (defun key-stroke-update (event)
		      (if (or (= (ps:chain event char-code) 13)
			      (= (ps:chain event key-code) 13))
			  (post-chat-msg)))

		    (defun post-chat-msg () 
		      ($.post
		       (concatenate 'string "/blog/chat/i/" *chat-id*)
		       (session-obj 
			:message 
			(val-of "input#message")))
		      (ps:chain ($ "input#message") (val ""))))))))

(page-handler  tb-logged-out)
(page-handler  tb-logged-in)
(page-handler tb-logged-in-basic)
(page-handler tb-not-friend)
(page-handler tb-is-friend)
(page-handler tb-logged-in-friend-feed)
(page-handler  tb-logged-in-friends)
(page-handler tb-logged-in-followers)
(page-handler  tb-logged-in-chat )
(page-handler tb-logged-in-chat-history)
(page-handler tb-logged-in-post)
(page-handler tb-logged-in-settings)



(defhandler (blog get ("topbar" "other")) (:|html|)
  (bind-query () ((session-id "session-id")
		  (author "author"))
    (let ((user (check-login session-id)))
      (if user
	  (if (friend-p user author)
	      (reply (with-output-to-string (stream)
		       (render-page tb-is-friend stream)))
	      (reply (with-output-to-string (stream)
		       (render-page tb-not-friend stream))))
	  (reply (with-output-to-string (stream)
		   (render-page tb-logged-out stream)))))))

(defun hero-header (stream title subtitle)
  (cl-who:with-html-output (stream)
    (:div :class "page-header"
	  (:h2 :id "title" :name "title" (cl-who:str title))
	  (:p :id "subtitle" :name "subtitle" (cl-who:str subtitle)))))

(defhandler (blog get ("index")) (:|html|)
  (let* ((users (with-recursive-connection ()
		  (redis:red-smembers *users*)))
	 (user-names (with-recursive-connection ()
		       (redis:with-pipelining 
			 (dolist (user users)
			   (hgetredis user "display-name" *settings-ns*)))))
	 (titles (with-recursive-connection ()
		   (redis:with-pipelining 
		     (dolist (user users)
		       (hgetredis user "title" *settings-ns*)))))
	 (html (cl-who:with-html-output-to-string (var)
		 (:html (:head (:title "Welcome to Multiblog")
			       (:link :rel "stylesheet" :href "/bootstrap.css")
			       (:style :type "text/css"  "body {padding-top: 60px;}"))
			(:body 
			 (:script :src "/jquery.min.js")
			 (:script :src "/blog/jslib/")
			 (:script :type "text/javascript"
				  (cl-who:str
				   (ps:ps 
				     (ps:chain 
				      ($ document) 
				      (ready
				       (lambda ()
					 (init-login)))))))
			 (:div :id "topbar" (render-page tb-logged-out var))
			 (:div :class "container"
			       (hero-header var  "Multiblog:"  "Swansong of the internet")
				     (:h3 "Blogs:")
			       (map nil (lambda (user name title)
					  (cl-who:htm (:h4 (:a :href (format nil "/blog/main/~a" user)
							       (cl-who:str (format nil "~a: ~a" name title)))) :br :br))
				    users user-names titles)))))))
    (reply html)))

(defhandler (blog get ("main" author)) (:|html|)
  (setf author (string-downcase author))
  (let ((properties (hmgetredis author *settings-ns*)))
    (reply 
     (cl-who:with-html-output-to-string (var)
       (:html (:head (:title (cl-who:str (getprop properties "title")))
		     (:link :rel "stylesheet" :href "/bootstrap.css")
		     (:style :type "text/css"  "body {padding-top: 60px;}"))
	      
	      (:body 
	       (:div :id "topbar" (render-page tb-logged-out var))
	       (:div :class "container"
		     (hero-header var 
				  (getprop properties "title")
				  (getprop properties "subtitle"))
				  
		     (:div :class "row"
			   (:div :id "index" :class "span4" "Index Goes Here")
			   (:div :id "blog" :class "span8" "Body Text Goes Here")
			   (:div :id "chat" :class "span4" "Chat Goes Here")))

	       (:script :src "/jquery.min.js")
	       (:script :src (format nil "/blog/jslib/~a" author))

	       (cl-who:htm
		(:script :type "text/javascript"
			 (cl-who:str
			  (ps:ps 
			    (ps:chain 
			     ($ document) 
			     (ready
			      (lambda ()
				(get-init-post)
				(init-login)
						  
				
				#|(poll-index)|#
				))))))
		(:input :type "hidden" :id "session-id" :name "session-id"))))))))

(defhandler (blog get ("post" "new")) (:|html|)
  (bind-query () ((session-id "session-id")
		  (reply-to "reply-to"))
      (reply (cl-who:with-html-output-to-string (var)
	       (:html (:body
		       (:h3 "Create a new post")
		       (:script :src "/showdown.js")
		       (:script :type "text/javascript"
				(cl-who:str 
				 (ps:ps
				   (defvar *converter* (ps:new (ps:chain -showdown (converter))))
				   (defun markdownize (event)
				     (ps:chain ($ "div#preview") 
					       (html 
						(ps:chain *converter*
							  (make-html (val-of "textarea#body")))))))))
		       :br
		       "Title"
		       (:input :type "text" :name "title" :id "title")
		       (when reply-to 
			 (cl-who:htm
			  (:input :type "hidden" :name "reply-to" :id "replyto" :value reply-to)))
		       :br
		       "Text"
		       :br
		       (:textarea :onkeypress (ps:ps-inline (markdownize event))
				  :style "width: 444px; height: 178px;" :name "body" :id "body")
		       :br
		       (:input :type "submit" :value "Submit" :onclick
			       (cl-who:str
				(if reply-to
				    (ps:ps-inline 
				     (make-post 
				      (val-of "input#title")
				      (val-of "textarea#body")
				      (val-of "input#reply-to")))
				    (ps:ps-inline 
				     (make-post 
				      (val-of "input#title")
				      (val-of "textarea#body"))))))
		       (:div :id "preview" :name "preview")))))))



(defhandler (blog post ("post" "new")) (:|content| "application/json")
    (bind-query () ((session-id "session-id")
		    (title "title")
		    (post "post")
		    (reply-to "reply-to"))
      
    (let ((user (check-login session-id)))
      (if (and (and user title post) (has-textp title) (has-textp post))	
	  (let ((pst-id (generate-post-entry title user post)))
	    (when reply-to
	      (add-post-reply reply-to pst-id))
	    (reply-status "success" 
			  "user" user
			  "postId" pst-id))
	  (reply-status "failure" "user" "" "postId" "")))))

(defhandler (blog get ("post" "edit")) (:|html|)
  (bind-query ()
      ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if user
	  (with-recursive-connection ()
	    (let ((post-ids (redis:red-zrange (predicate user *pst-idx*) 0 -1)))
	      (let ((post-titles (redis:with-pipelining 
				   (dolist (id post-ids)
				     (hgetredis id "title" *pst-ns*)))))
		(let ((output-string (cl-who:with-html-output-to-string (var)
				       (:h3 "Select A Post to Edit")
				       (map nil (lambda (id title) 
						  (cl-who:htm
						   (:h4 (cl-who:str title)) :br
						   (named-link var "Edit"
							       (format nil "/blog/post/edit/~a" id)
							       "div#blog"
							       '(lambda ()) 
							       '(ps:create
								 :session-id (ps:chain ($ "input#session-id") (val))))
						   "|"
						   (named-link var "Remove"
							       (format nil "/blog/post/remove/~a" id)
							       "div#blog"
							       '(lambda ()) 
							       '(ps:create
								 :session-id (ps:chain ($ "input#session-id") (val))))
						   :hr))
					    post-ids post-titles))))
		  (reply output-string)))))
	  (reply "error")))))

#|(defhandler (blog get ("post" "remove")) (:|html|)
  (bind-query ()
      ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if user
	  (with-recursive-connection ()
	    (let ((post-ids (redis:red-zrange (predicate user *pst-idx*) 0 -1)))
	      (let ((post-titles (redis:with-pipelining 
				   (dolist (id post-ids)
				     (hgetredis id "title" *pst-ns*)))))
		(let ((output-string (cl-who:with-html-output-to-string (var)
				       (:h3 "Select A Post to Remove")
					     (:ul 
					      (map nil (lambda (id title) 
							 (clickable-li var title
								       (format nil "/blog/post/remove/~a" id)
								       "div#blog"
								       '(lambda ()) 
								       '(ps:create
									 :session-id (ps:chain ($ "input#session-id") (val)))))
						   post-ids post-titles)))))
		  (reply output-string)))))
	  (reply "error")))))|#


(defhandler (blog get ("post" "remove" post-id)) (:|html|)
  (bind-query ()
      ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if (and user 
	       (zscore user *pst-idx* post-id))
	  (reply (concatenate 'string
			      (cl-who:with-html-output-to-string (stream)
				  (:button :class "btn" 
					   :type "submit"
					   :onclick (ps:ps-inline* `(delete-post ,post-id)) 
					   "Delete This Post"))
			      (generate-post-from-db post-id)))
	  (reply "error")))))

(defhandler (blog post ("post" "remove")) (:|content| "application/json")
  (bind-query ()
      ((session-id "session-id")
       (post-id "post-id"))
    (let ((user (check-login session-id)))
      (if (and user (zscore user *pst-idx* post-id))
	  (progn
	    (remove-post-entry user post-id)
	    (reply-status "success"
			  "postId" post-id))
	  (reply-status "failure"
			"postId" post-id)))))


(defhandler (blog get ("post" "edit" post-id)) (:|html|)
  (bind-query ()
      ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if (and user (zscore user *pst-idx* post-id))
	  (let ((post (hmgetredis post-id *pst-ns*)))
	    (destructure-props ((title "title")
				(body "body"))
		post
	      (reply (cl-who:with-html-output-to-string (var)
		       (:html (:body
			       (:h3 "Edit a Post")
			       (:script :src "/showdown.js")
			       (:script :type "text/javascript"
					(cl-who:str 
					 (ps:ps
					   (defvar *converter* (ps:new (ps:chain -showdown (converter))))
					   (defun markdownize (event)
					     (ps:chain ($ "div#preview") (html (ps:chain *converter* (make-html (val-of "textarea#body")))))))))
			       :br
			       "Title"
			       (:input :type "text" :name "title" :id "title" :value (cl-who:str title))
			       :br
			       "Text"
			       :br
			       (:textarea :onkeypress (ps:ps-inline (markdownize event))
					  :style "width: 444px; height: 178px;" :name "body" :id "body" (cl-who:str body))
			       :br
			       (:input :type "submit" :value "Submit" :onclick
				       (ps:ps-inline 
					(update-post 
					 (val-of "input#title")
					 (val-of "textarea#body")
					 (val-of "input#post-id"))))
			       (:div :id "preview" :name "preview")
			       (:input :type "hidden" :name "post-id" :id "post-id" :value (cl-who:str post-id))))))))
	  (reply "error")))))

(defhandler (blog post ("post" "edit")) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (title "title")
		  (post "post")
		  (post-id "post-id"))
    (let ((user (check-login session-id)))
      (if (and 
	   (and user title post post-id)
	   (zscore user *pst-idx* post-id)
	   (has-textp title)
	   (has-textp post))
	  (progn
	    (update-post-entry post-id title user post)
	    (reply-status "success" 
			  "user" user
			  "postId" post-id))
	  (reply-status "failure" "user" "" "postId" "")))))

(defhandler (blog get ("friend-feed")) (:|html|)
  (bind-query () 
      ((session-id "session-id")
       (start "start")
       (end "end"))
    (setf start (safe-parse-int start)
	  end (safe-parse-int end))
    (let ((user (check-login session-id)))
      (if (and user start end)
	  (with-recursive-connection ()
	    (let* ((paths (redis:red-zrange (predicate user *mailbox-ns*) start end))
		   (post-infos (redis:with-pipelining 
				 (dolist (path paths)
				   (redis:red-hmget (predicate path *pst-ns*) "title" "body" "author")))))
	      (if paths
		  (reply 
		   (cl-who:with-html-output-to-string (var)
		     (:h4 "Your Feed")
		     (:input :type "hidden" :name "feed-start" :id "feed-start" :value (cl-who:str start))
		     (:input :type "hidden" :name "feed-end" :id "feed-end" :value (cl-who:str end)) 
		     (map 'nil 
			  (lambda (postid post-info)
			    (destructuring-bind (title body author) post-info
			      (when (and postid title)
				(cl-who:htm 
				 (:h2 (named-link var title (format nil "/blog/viewpost/~a" postid) "div#blog"
						  '(lambda () (topbar-swap tb-logged-in-friends))))
				 (:p
				  (cl-who:str 
				   (if (> (length body) 140)
				       (concatenate 'string (subseq body 0 140) "...")
				       body)))
				 (:a :href (format nil "/blog/main/~a" author) (cl-who:str author))
				 :br
				 (named-link var "Reply" 
					     "/blog/post/new/"
					     "div#blog"
					     '(lambda ()) `(session-obj "reply-to" ,postid))
				 :br
				 :hr))))
			  paths post-infos)))
		  (reply "Add Some Friends to See Something Here!"))))
	  (reply  "error")))))

(defhandler (blog get ("settings")) (:|html|)
   (bind-query () ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if user
	  (let* ((settings (hmgetredis user *settings-ns*)))
	    (let ((title (getprop settings "title" "Title"))
		  (display-name (getprop settings "display-name" user))
		  (subtitle (getprop settings "subtitle" "Subtitle")))

	      (reply (cl-who:with-html-output-to-string (var)
		       (:body
			"Display Name" :br
			(:input :type "text" :id "display-name" :name "display-name" :value display-name) :br
			"Title" :br
			(:input :type "text" :id "title" :name "title" :value title) :br
			"Subtitle" :br
			(:input :type "text" :id "subtitle" :name "subtitle" :value subtitle) :br
			(:input 
			 :type "submit"
			 :name "submit"
			 :onclick (cl-who:str (ps:ps-inline (update-settings
							     (session-obj
							      :display-name 
							      (val-of "input#display-name")
							      :title
							      (val-of "input#title")
							      :subtitle
							      (val-of "input#subtitle")))))
			 :value "Update Settings"))))))
	  (reply "Log In First")))))

(defhandler (blog post ("settings")) (:|content| "application/json")
  (bind-query (q) ((session-id "session-id")
		   (title "title")
		   (subtitle "subtitle")
		   (display-name "display-name"))
    (let ((user (check-login session-id)))
      (if user
	  (if (and (has-textp title) (has-textp display-name))
	      (progn
		(hmsetredis user *settings-ns* "title" title "subtitle" subtitle "display-name" display-name)
		(reply-status "success" "title" title "subtitle" subtitle "display-name" display-name))
	      (reply-status "failure"))
	  (reply-status "failure")))))

(defhandler (blog get ("register")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:title "Registration")
		  (:body (:B "Register to Post and Chat")
			 (:form :action "/blog/register" :method "POST"
				"UserId"
				:br
				(:input :type "text" :name "user")
				:br
				"Password"
				:br
				(:input :type "password" :name "password")
				:br
				"Password Again"
				:br
				(:input :type "password" :name "password2")
				:br
				"Title"
				:br
				(:input :type "text" :name "title" :value "Your Title Here")
				:br
				"Subtitle"
				:br
				(:input :type "text" :name "subtitle" :value "Your Subtitle Here")
				:br
				(:input :type "submit" :value "Register")))))))

(defhandler (blog post ("register")) (:|html|)
  (bind-query () ((user "user")
		  (password "password")
		  (password2 "password2")
		  (title "title")
		  (subtitle "subtitle"))
    (setf user (string-downcase user))
    (cond 
      ((or (getredis user *password-ns*)
	   (< (length user) 3))
       (reply (cl-who:with-html-output-to-string (var)
		(:html (:body (:B "Name already taken or name must be at least 3 characters")
			      :br (:b (:a :href "/blog/register" "Try Again")))))))
      ((and user (string= password password2))
       (with-recursive-connection ()
	 (redis:red-sadd *users* user)
	 (add-password user password)
	 (generate-post-entry "Default Post" user "First Post")
	 (let ((chat-id (generate-chat-entry "Default Chat" user)))
	   (hsetredis user "default-chat" chat-id *settings-ns*))
	 (hmsetredis user *settings-ns*
		     "title" title
		     "subtitle" subtitle "display-name" user))
       (reply (format nil "/blog/main/~a" user) :|redirect|)))))

(defhandler (blog post ("login")) (:|content| "application/json")
  (bind-query () ((user "user")
		  (password "password"))
    
    (setf user (string-downcase user))
          
    (let ((uuid (create-login user password)))
      (if uuid 
	  (reply-status "success" 
			"expires" *expire-days*
			"cookieId" *site-cookie-name*
			"postId" (most-recent-post user)
			"sessionId" uuid)
	  (reply-status "failure")))))

(defhandler (blog post ("logout")) (:|content| "application/json")
  (bind-query () ((session-id "session-id"))
    (timeout-session session-id)
    (reply-status "success")))


(defhandler (blog post ("re-auth")) (:|content| "application/json")
  (bind-query () ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if user
	  (reply-status "success" "user" user)
	  (reply-status "failure" "user" "")))))

(let ((chat-reply-table (make-hash-table :test  #'equalp :synchronized t))
      (lock (bt:make-lock))
      (condition-var (bt:make-condition-variable))
      (reply-count 0)
      (max-queued-replies 500))

  (defun get-chat-text (chat-id &optional (chat-length 20))
    (let* ((text-list (lrangeredis chat-id *chat-ns* 0 chat-length))
	  (chat-info (hmgetredis chat-id *chat-info*))
	  (title (getprop chat-info "title")))
      (cl-who:with-html-output-to-string (var)
	(:h1 (cl-who:str title))
	(cl-who:str (apply #'concatenate 'string (nreverse text-list))))))

  (defun set-chat-text (chat-id string)
    (lpushredis chat-id *chat-ns* string)
    (reply-chat chat-id))

  (defun queue-request (chat-id) 
    (sb-ext:with-locked-hash-table (chat-reply-table)
      (push (get-reply-information) (gethash chat-id chat-reply-table nil))
      (if (>= reply-count max-queued-replies)
	  (bt:with-lock-held (lock)
	    (bt:condition-notify condition-var))
	  (incf reply-count))))

  (defun reply-all-chats ()
    (sb-ext:with-locked-hash-table (chat-reply-table)
      (decf reply-count reply-count) 
      (maphash (lambda (chat-id chat-reply-list)
		 (let ((text (get-chat-text chat-id)))
		   (reply-all text chat-reply-list :|html| nil)))
	       chat-reply-table)))

  (defun init-reply-chat-thread ()
    (bt:make-thread (lambda ()
		      (bt:with-lock-held (lock)
			(do ()
			    (NIL)
			  (bt:condition-wait condition-var lock)
			  (reply-all-chats))))))


  (defun reply-chat (chat-id)
    (let ((chat-reply-list (sb-ext:with-locked-hash-table (chat-reply-table)
			     (prog1 (gethash chat-id chat-reply-table)
			       (setf (gethash chat-id chat-reply-table) nil)))))
      (let ((text (get-chat-text chat-id)))
	(reply-all text  chat-reply-list :|html| nil)))))

(defhandler (blog get ("chat" "i" chat-id)) (:|html|)
  (bind-query () ((session-id "session-id"))
    (if (check-login session-id)
	(reply (cl-who:with-html-output-to-string (val)
		 (:html (:body 
			 (:script :type "text/javascript"
				  (cl-who:str 
				   (ps:ps*
				    `(progn
				       (setf *chat-id* ,chat-id)
				       (chat-loop-init)))))
			 :br
			 (:div :id "chatwindow")
			 :br
			 "Message: "
			 (:input 
			  :id "message"
			  :type "text"
			  :name "message"
			  :onkeypress (ps:ps-inline (key-stroke-update event)))
			 :br
			 (:input :type "submit" 
				 :value "Send"
				 :onclick (ps:ps-inline* `(post-chat-msg ,chat-id)))))))
	(reply 
	 (cl-who:with-html-output-to-string (val)
	   (:html (:body 
		   (:script :type "text/javascript"
			    (cl-who:str (ps:ps* `(progn
						   (setf *chat-id* ,chat-id)
						   (chat-loop-init)))))
		   (:div :id "chatwindow"))))))))

(defhandler (blog post ("chat" "i" chat-id)) (:|html|)
  (reply "")
  (bind-query () ((session-id "session-id")
		  (message "message"))
    (let ((name (check-login session-id)))
      (when (and name message)
	(set-chat-text
	 chat-id (cl-who:with-html-output-to-string (str)
		   (:span :class "timestamp" (cl-who:str (timestamp))) " " 
		   (:a :href (format nil "/blog/main/~a" name) 
		       (cl-who:str (format nil "~a:" (hgetredis name "display-name" *settings-ns*))))
		   " "
		   (cl-who:str message)
		   :br))))))

(defhandler (blog get ("chat" "create")) (:|html|)
  (reply (cl-who:with-html-output-to-string (str)
	   "Chat Name" 
	   :br
	   (:input 
	    :id "chat-name"
	    :name "chat-name"
	    :type "text")
	   :br
	   (:input :type "submit"
		   :value "Create"
		   :onclick (ps:ps-inline (create-chat))))))

(defhandler (blog post ("chat" "create")) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (chat-name "chat-name"))
    (let ((user (check-login session-id)))
      (if user
	  (let ((chat-id (generate-chat-entry chat-name user)))
	    (reply-status "success" "chat-id" chat-id))
	  (reply-status "failure")))))

(defhandler (blog get ("chat" "wait" chat-id)) (:|html|)
      (queue-request chat-id))

(defhandler (blog get ("chat" "instant" chat-id)) (:|html|)
    (reply (get-chat-text chat-id)))

(defhandler (blog get ("chat" "subs")) (:|html|)
  (bind-query () ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if user
	  (reply (cl-who:with-html-output-to-string (var)
		   (:h1 "Your Chat Subs") 
		   :br
		   (let ((counter 0))
		     (multiple-value-bind (ids titles owners) (chat-subs user)
		       (map nil (lambda (id title owner)
				  (cl-who:htm (:h4 (cl-who:str title)) :br
					      (named-link var "View" (format nil "/blog/chat/i/~a" id) "div#chat" '(lambda ()) '(session-obj))
					      " "
					      (:a :href "#" :onclick (ps:ps-inline* `(progn (chat-history ,id 0 20)
											    (topbar-swap tb-logged-in-chat-history))) "History")
					      (when (equalp owner user)
						(cl-who:htm " ")
						(named-link var "Edit" (format nil "/blog/chat/edit/~a" id) "div#blog"
							    '(lambda ()) '(session-obj)))
					      " "
					      (:a :href "#" :onclick (ps:ps-inline* `($.post ,(format nil "/blog/chat/setdefault/~a" id)
											     (session-obj)))
						  "Set as Default")
					      :hr) (incf counter))
			    ids titles owners)))))
	  (reply "")))))

(defhandler (blog post ("chat" "setdefault" chat-id)) (:|content| "application/json")
  (bind-query () ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if (and user (chat-owned-p  user chat-id))
	  (progn (hsetredis user "default-chat" chat-id *settings-ns*)
		 (reply-status "success"))
	  (reply-status "failure")))))

(defhandler (blog get ("chat" "owned" owner)) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:h1 (format nil "~a's Chat Subs" owner)) 
	   :br
	   (let ((counter 0))
	     (multiple-value-bind (ids titles) (owned-chats owner)
	       (map nil (lambda (id title)
			  (cl-who:htm (:h4 (cl-who:str title)) :br
				      (named-link var "View" (format nil "/blog/chat/i/~a" id) "div#chat")
				      " "
				      (:a :href "#" :onclick (ps:ps-inline* `(progn (chat-history ,id 0 20)
										    (topbar-swap tb-logged-in-chat-history))) "History")
				      (cl-who:htm " ")
				      (named-link var "Edit" (format nil "/blog/chat/edit/~a" id) "div#blog"
						  '(lambda ()) '(session-obj))
				      (cl-who:htm " ")
				      (:a :href "#" :onclick (ps:ps-inline* `($.post ,(format nil "/blog/chat/setdefault/~a" id)
										     (session-obj)))
					  "Set as Default")
				      :hr
				      ) (incf counter))
		    ids titles))))))

(defhandler (blog get ("chat" "edit" id)) (:|html|)
  (bind-query () ((session-id "session-id"))
    (let* ((user (check-login session-id))
	   (chat-info (hmgetredis id *chat-info*))
	   (owner (getprop chat-info "owner")))
      (if (and user owner (equalp owner user))
	  (let ((title (getprop chat-info "title")))
	    (reply (cl-who:with-html-output-to-string (var)
		     (:h1 "Edit Chat Settings")
		     :br
		     (:input :type "text" :id "ch-name" :name "ch-name" :value title)
		     :br
		     (:input :type "checkbox" :id "checkbox" :name "checkbox" :value "true")
		     :br
		     (:input :type "submit" :onclick (ps:ps-inline* `($.post 
								      ,(format nil "/blog/chat/edit/~a" id)
								      (session-obj
								       "title" (val-of "input#ch-name")
								       ;"default" 
								       ;(ps:chain ($ "input#checkbox") value)
								       )))
			     :value "Submit"))))
	  (reply " ")))))

(defhandler (blog post ("chat" "edit" id)) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (title "title")
		  (default "default"))
    (let ((user (check-login session-id)))
      (if (and user (chat-owner-p user id))
	  (progn (hsetredis id "title" title *chat-info*)
		 (reply-status "success"))
	  (reply-status "failure")))))

    
	      

(defhandler (blog get ("chat" "history" chat-id)) (:|content| "application/json")
  (bind-query () ((start "start")
		  (end "end")
		  (title "title"))
    (if (and start end)
	(reply-status 
	 "success"
	 "result"
	 (cl-who:with-html-output-to-string (var)
	   (:h3 (cl-who:str title))
	   (:input :type "hidden" :id "ch-start" :name "ch-start" :value start)
	   (:input :type "hidden" :id "ch-end" :name "ch-end" :value end)
	   (cl-who:str (apply #'concatenate 'string (nreverse (lrangeredis chat-id *chat-ns* start end))))))
	(reply-status "failure"))))
			      




(defhandler (blog post ("friends" "json" "remove")) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (friends "friends"))
    (let ((user (check-login session-id)))
      (if (and user friends)
	  (let ((friends-list (cl-ppcre:split " " friends)))
	    (dolist (friend friends-list)
	      (remove-friend user friend))
	    (reply-status "success"))	    
	  (reply-status "failure")))))


(defhandler (blog post ("friends" "json" "add")) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (friends "friends"))
    (let ((user (check-login session-id)))
      (if (and user friends)
	  (let ((friends-list (cl-ppcre:split " " friends)))
	    (dolist (friend friends-list)
	      (add-friend user friend))
	    (reply-status "success"))
	  (reply-status "failure")))))

(defhandler (blog get ("friends")) (:|html|)
  (bind-query () ((session-id "session-id")
		  (user "user"))
    (if user
	(let* ((friends-list (get-friends user))
	       (display-names 
		(with-recursive-connection ()
		  (redis:with-pipelining
		    (map 'nil (lambda (friend) (hgetredis friend "display-name" *settings-ns*))
			 friends-list)))))
	  (if friends-list
	      (reply 
	       (cl-who:with-html-output-to-string (var)
		 (map nil (lambda (friend display-name)
			    (cl-who:htm (:a :href (format nil "/blog/main/~a" friend) (cl-who:str display-name)) :br))
		      friends-list display-names)))
	      (reply "Add Some Friends to See Something Here!")))
	(reply ""))))

(defhandler (blog get ("friends" "json" "list")) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (user-to-lookup "user"))
    (format t "~s~%" user-to-lookup)
    (let ((user (check-login session-id))
	  (friends-list  (get-friends user-to-lookup)))
      (if (and user user-to-lookup friends-list)
	  (reply-status "success" "friends" friends-list)
	  (reply-status "failure")))))

(defhandler (blog get ("followers" "json" "list")) (:|content| "application/json")
  (bind-query () ((session-id "session-id")
		  (user-to-lookup "user"))
    (format t "~s~%" user-to-lookup)
    (let ((user (check-login session-id))
	  (followers-list  (get-followers user-to-lookup)))
      (if (and user user-to-lookup followers-list)
	  (reply-status "success" "followers" followers-list)
	  (reply-status "failure")))))

(defhandler (blog get ("followers")) (:|html|)
  (bind-query () ((session-id "session-id")
		  (user "user"))
    (if user
	(let* ((followers-list (get-followers user))
	       (display-names 
		(with-recursive-connection ()
		  (redis:with-pipelining
		    (map 'nil (lambda (follower) (hgetredis follower "display-name" *settings-ns*))
			 followers-list)))))
	  (if followers-list
	      (reply 
	       (cl-who:with-html-output-to-string (var)
		 (map nil (lambda (follower display-name)
			    (cl-who:htm (:a :href (format nil "/blog/main/~a" follower) (cl-who:str display-name)) :br))
		      followers-list display-names)))
	      (reply "Gain Some Followers to See Something Here!")))
	(reply ""))))
	
(generate-appmods)

(defun blog-main ()
  ;;can go into redis later on.

  (setf *yaws-server-node-name* "socrates")
  (setf *cookie-file* "/home/jon/github/Lisp-on-Yaws/COOKIE")
  

  (with-recursive-connection ()
    (setf *site-cookie-name* (let ((cookie  (redis:red-get "SITE:COOKIE")))
			       (if cookie 
				   cookie
				   (let ((uuid (uuid-string))) 
				     (redis:red-set "SITE:COOKIE" uuid)
				     uuid))))
    (unless (redis:red-get *post-counter*) (redis:red-set *post-counter* 1)))
  (init-server-connection)
					;(setf *reply-chat-thread* (init-reply-chat-thread))
  (generate-appmods))