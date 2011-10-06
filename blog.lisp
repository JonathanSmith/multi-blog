(in-package "BLOG")

(defvar *site-cookie-name*)
(defvar *domain-root* "0.0.0.0:8080")

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
;(init-appmod blog)

(defhandler (blog get ("viewpost" postid)) (:|html|)
  (if postid
      (reply (generate-post-from-db postid))
      (reply "")))

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

(defhandler (blog get ("jslib" author)) (:|content| "application/javascript")
  (setf author (string-downcase author))
  (reply 
   (concatenate 'string  
		(let ((link (format nil "/blog/viewpost/~a" (most-recent-post author))))
		  (ps:ps* 
		   `(defvar author ,author)
		   `(defun get-init-post ()
		      (js-link ,link "div#blog"))
		   `(defun init-login ()
		      (let ((session-id (get-cookie ,*site-cookie-name*)))
			($.post "/blog/re-auth/" (ps:create :session-id session-id)
				(lambda (data textstatus qxhr)
				  (if (equal (ps:getprop data 'status) "success")
				      (progn
					(ps:chain ($ "input#session-id") (val session-id))
					(after-login (ps:getprop data 'user))
					(js-link "/blog/chat/" "div#chat" chat-loop-init))
				      (js-link "/blog/login/" "div#login"))))))
		   `(defun log-out ()
		      (set-cookie ,*site-cookie-name* "" 0)
		      (ps:chain ($ "div#chat") (html ""))
		      (init-login))

		   `(defun update-index ()
		      ($.get ,(format nil "/blog/index/~a" author)
			     (ps:create :start 0 :end 20)
			     (lambda (data)
			       (ps:chain 
				($ "div#index")
				(html data)))))

		   `(defun check-last-post ()
		      ($.get ,(format nil "/blog/last_post/~a" author)  
			     (ps:create)
			     (lambda (server-id)
			       (let ((this-id (ps:chain ($ "input#latest") (val))))
				 (unless (equal this-id server-id)
				   (update-index)
				   (ps:chain ($ "input#latest") (val server-id)))))
			     "json"))))
		(ps:ps
		  (defun after-login (user)
		    (let ((a (concatenate 'string  "Logged In As " user))
			  (b (ps:lisp (cl-who:with-html-output-to-string (s) 
					:br
					(:a :href "#" :onclick (ps:ps-inline (log-out)) "Log Out")))))
		      (ps:chain ($ "div#login") (html (concatenate 'string a b)))
		      (when (equal user author)
			(ps:chain 
			 ($ "div#menu") 
			 (html 
			  (ps:lisp 
			   (cl-who:with-html-output-to-string (var)
			     (:ul
			      (clickable-li var "Add A Post" "/blog/post/new" "div#blog")
			      (clickable-li var "Edit a Post" "/blog/post/edit" "div#blog" '(lambda ()) '(ps:create 
													  :session-id (ps:chain ($ "input#session-id") (val))
													  :start 0
													  :end 20))
			      (clickable-li var "View Feed" "/blog/friend-feed/" "div#blog"
					    '(lambda ())
					    '(ps:create 
					      :session-id (ps:chain ($ "input#session-id") (val))
					      :start 0
					      :end 20))
			      (clickable-li var
					    "Settings"
					    "/blog/settings"
					    "div#blog" 
					    '(lambda ())
					    '(ps:create :session-id (ps:chain ($ "input#session-id") (val)))))))))
		        )))

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
					 #.(format nil ";~%expires=")
					 (ps:chain exdate (to-u-t-c-string)))))))
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
				
		  (defun poll-index ()
		    (ps:var timer (set-interval "checkLastPost()" 30000)))

		  (defpostfn make-post (blog post new)
		    ((session-id title text)
		     (ps:create "session-id" session-id
				"title" title
				"post" text))
		    ((data textstatus qxhr)
		     (let ((status (ps:getprop data 'status)))
		       (if (equal status "success")
			   (let* ((most-recent-post (ps:getprop data 'post-id))
				  (user (ps:getprop data 'user))
				  (posts-link (concatenate 'string
							   "/blog/viewpost/" most-recent-post))
				  (indexes-link (concatenate 'string "/blog/index/" user)))
			     (js-link posts-link "div#blog")
			     (js-link "/blog/chat/" "div#chat" chat-loop-init)
			     (js-link indexes-link "div#index")
			     (ps:chain ($ "div#notify") (html (concatenate 'string "Post Success!"))))

			   (ps:chain ($ "div#notify") 
				     (html (concatenate 'string "Post Failure!")))))))

		  (defpostfn update-post (blog post edit)
		    ((session-id title text id)
		     (ps:create "session-id" session-id
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
			     (js-link posts-link "div#blog")
			     (js-link "/blog/chat/" "div#chat" chat-loop-init)
			     (js-link indexes-link "div#index")
			     (ps:chain ($ "div#notify") (html (concatenate 'string "Post Success!"))))

			   (ps:chain ($ "div#notify") 
				     (html (concatenate 'string "Post Failure!")))))))

		  (defpostfn update-settings (blog settings)
		    ((object) object)
		    ((data texstatus qxhr)
		     (let ((status (ps:getprop data 'status)))
		       (if (eql status "success")
			   (ps:chain ($ "div#notify") (html "Settings Updated"))
			   (ps:chain ($ "div#notify") (html "Settings Not Upated"))))))

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
			   (after-login user)
			   (js-link "/blog/chat/" "div#chat" chat-loop-init))))))

		  (defpostfn add-friend (blog friends json add)
		    (()
		     (ps:create "session-id" (ps:chain ($ "input#session-id") (val))
				"friends" author))
		    ((data texstatus qxhr)))

		  (defun chat-loop-init ()
		    ($.get (concatenate 'string "/blog/chat/instant/" author)
			   (ps:create :session-id 
				      (ps:chain ($ "input#session-id")
						(val)))
			   (lambda (data)
			     (ps:chain ($ "div#chatwindow")
				       (html data))
			     (chat-loop))))
		  (defun chat-loop ()
		    ($.get (concatenate 'string "/blog/chat/wait/" author)
			   (ps:create :session-id 
				      (ps:chain ($ "input#session-id")
						(val)))
			   (lambda (data)
			     (ps:chain ($ "div#chatwindow") 
				       (html data))
			     (chat-loop))))
			       
		  (defun chat-history (start end)
		    ($.get (concatenate 'string "/blog/chat/history/" author)
			   (ps:create :session-id (ps:chain ($ "input#session-id") (val))
				      :start start
				      :end end)
			   (lambda (data)
			     (let ((status (ps:getprop data 'status)))
			       (if (eql status "success")
				   (let ((result (ps:getprop data 'result)))
				     (ps:chain ($ "div#blog")
					       (html result))))))))

		  (defun key-stroke-update (event)
		    (if (or (= (ps:chain event char-code) 13)
			    (= (ps:chain event key-code) 13))
			(post-chat-msg)))

		  (defun post-chat-msg () 
		    ($.post
		     (concatenate 'string "/blog/chat/p/" author)
		     (ps:create 
		      :message 
		      (ps:chain 
		       ($ "input#message")
		       (val))
		      :session-id
		      (ps:chain 
		       ($ "input#session-id")
		       (val))))
		    (ps:chain ($ "input#message") (val "")))))))

(defun top-bar (stream)
  (cl-who:with-html-output (stream)
    (:div 
     :class "topbar"
     (:div 
      :class "fill"
      (:div
       :class "container"
       (:a :class "brand" :href "/blog/register" "Register For Multiblog")
       (:ul 
	:class "nav"
	(:li :class "active" (:a :href "/blog/main" "Home"))
	(:li (:a :href "#" "About"))
	(:li (:a :href "/blog/main/jons" "Contact")))
       (:form 
	:action "" :class "pull-right"
	(:input :class "input-small" :type "text" :placeholder "Username")
	(:input :class "input-small" :type "password" :placeholder "Password")
	(:button :class "btn" :type "submit" "Sign In")))))))

(defhandler (blog get ("main")) (:|html|)
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
			       (:link :rel "stylesheet" :href "/styles.css"))
			(:body 
			 (top-bar var)
			 :br
			 (:div :class "container"
			       (:h1 "Multiblog:")
			       (:h2 "Swansong of the internet")
			       :br
			       (:h3 "Blogroll:")
			       (map nil (lambda (user name title)
					  (cl-who:htm (:h4 (:a :href (format nil "/blog/main/~a" user) (cl-who:str (format nil "~a: ~a" name title)))) :br :br))
				    users user-names titles)))))))
		     
    (reply html)))

(defhandler (blog get ("main" author)) (:|html|)
  (setf author (string-downcase author))
  (let ((properties (hmgetredis author *settings-ns*)))
    (reply 
     (cl-who:with-html-output-to-string (var)
       (:html (:head (:title (cl-who:str (getprop properties "title")))
		     ;(:meta :http-equiv "refresh" :content "1")
		     (:link :rel "stylesheet" :href "/blog.css"))
	      
	      (:body 
	       (:div :id "header" :class "header"
		     (:div  :id "notify" :class "notify")
		     (:div :id "login" :class "login")
		     (:h1 (cl-who:str (getprop properties "title")))
		     (:h4 (cl-who:str (getprop properties "subtitle"))))

	       (:div :id "index" :class "index")
	       (:div :id "chat" :class "chat")
	       (:div  :id "blog" :class "blog")	     
	  
	       (:div  :id "menu" :class "menu"
		      (:ul
		       
		       (clickable-li var "Friends" "/blog/friends" "div#blog"
				     '(lambda())
				     '(ps:create
				       :session-id (ps:chain ($ "input#session-id") (val))
				       :user author))
		       (cl-who:htm (:li :onclick (ps:ps-inline
						  (add-friend))
					"Add as Friend"))))

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
				(init-login)
				(get-init-post)		  
				(update-index)
				(poll-index)))))))
		(:input :type "hidden" :id "session-id" :name "session-id"))))))))

(defhandler (blog get ("post" "new")) (:|html|)
  (reply (cl-who:with-html-output-to-string (var)
	   (:html (:body
		   (:B "Not Much Here")		   
		   :br
		   "Title"
		   (:input :type "text" :name "title" :id "title")
		   :br
		   "Text"
		   :br
		   (:textarea :row "6" :cols "60" :name "post-text" :id "post-text")
		   :br
		   (:input :type "submit" :value "Submit" :onclick
			   (ps:ps-inline 
			    (make-post (ps:chain ($ "input#session-id")
						 (val))
				       (ps:chain ($ "input#title")
						 (val))
				       (ps:chain ($ "textarea#post-text")
						 (val))))))))))


(defhandler (blog post ("post" "new")) (:|content| "application/json")
    (bind-query () ((session-id "session-id")
		 (title "title")
		 (post "post"))
    (let ((user (check-login session-id)))
      (if (and (and user title post) (has-textp title) (has-textp post))	
	  (let ((pst-id (generate-post-entry title user post)))
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
	    (let ((post-ids (lrangeredis user *pst-idx* 0 -1)))
	      (let ((post-titles (redis:with-pipelining 
				   (dolist (id post-ids)
				     (hgetredis id "title" *pst-ns*)))))
		(let ((output-string (cl-who:with-html-output-to-string (var)
					     (:ul 
					      (map nil (lambda (id title) 
							 (clickable-li var title
								       (format nil "/blog/post/edit/~a" id)
								       "div#blog"
								       '(lambda ()) 
								       '(ps:create
									 :session-id (ps:chain ($ "input#session-id") (val)))))
						   post-ids post-titles)))))
		  (reply output-string)))))
	  (reply "error")))))

(defhandler (blog get ("post" "edit" post-id)) (:|html|)
  (bind-query ()
      ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if (and user (sismember user *pst-dict* post-id))
	  (let ((post (hmgetredis post-id *pst-ns*)))
	    (destructure-props ((title "title")
				(body "body"))
		post
	      (format t "~s~%" body)
	      (reply (cl-who:with-html-output-to-string (var)
		       (:html (:body
			       (:B "Not Much Here")		   
			       :br
			       "Title"
			       (:input :type "text" :name "title" :id "title" :value (cl-who:str title))
			       :br
			       "Text"
			       :br
			       (:textarea :row "6" :cols "60" :name "post-text" :id "post-text" (cl-who:str body))
			       
			       :br
			       (:input :type "submit" :value "Submit" :onclick
				       (ps:ps-inline 
					(update-post (ps:chain ($ "input#session-id")
							       (val))
						     (ps:chain ($ "input#title")
							       (val))
						     (ps:chain ($ "textarea#post-text")
							       (val))
						     (ps:chain ($ "input#post-id")
							       (val)))))
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
	   (sismember user *pst-dict* post-id)
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
	    (let* ((paths (lrangeredis user *mailbox-ns* start end))
		   (titles (mapcar #'first (redis:with-pipelining 
							   (dolist (path paths)
							     (redis:red-hmget (concatenate 'string *pst-ns* ":" path) "title"))))))
	      (reply (cl-who:with-html-output-to-string (var)
		       (when (>= start 20) 
			 (cl-who:htm (named-link var "Newer" "/blog/friend-feed" "div#blog"
						 '(lambda ())
						 `(ps:create
						   :session-id (ps:chain ($ "input#session-id") (val))
						   :start ,(- start 20)
						   :end ,(- end 20)))))
		       (cl-who:htm (named-link var "Older" "/blog/friend-feed" "div#blog"
					       '(lambda ())
					       `(ps:create
						 :session-id (ps:chain ($ "input#session-id") (val))
						 :start ,(+ start 20)
						 :end ,(+ end 20))) :br)
		       (map 'nil 
			    (lambda (path title)
			      (when (and path title)
				(cl-who:htm 
				 (named-link var title (format nil "/blog/viewpost/~a" path) "div#blog") :br))) paths titles)))))
	  (reply "")))))

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
							     (ps:create 
							      :session-id 
							      (ps:chain ($ "input#session-id")
									(val))
							      :display-name 
							      (ps:chain ($ "input#display-name")
									(val))
							      :title
							      (ps:chain ($ "input#title")
									(val))
							      :subtitle
							      (ps:chain ($ "input#subtitle")
									(val))))))
			 :value "Update Settings"))))))
	  (reply "Log In First"))
      )))

(defhandler (blog post ("settings")) (:|content| "application/json")
  #|(let* ((q (parse-query *query*))
  (session-id (second (assoc "session-id" q :test #'string=))))|#
  (bind-query (q) ((session-id "session-id"))
    (let ((user (check-login session-id)))
      (if user
	  (let ((title (cl-who:escape-string-iso-8859-1(second (assoc "title" q :test #'string=))))
		(subtitle (cl-who:escape-string-iso-8859-1 (second (assoc "subtitle" q :test #'string=))))
		(display-name (cl-who:escape-string-iso-8859-1 (second (assoc "display-name" q :test #'string=)))))
	    (if (and (has-textp title) (has-textp display-name))
		(progn
		  (hmsetredis user *settings-ns* "title" title "subtitle" subtitle "display-name" display-name)
		  (reply-status "success"))
		(reply-status "failure")))
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
				"E-mail"
				:br
				(:input :type "text" :name "e-mail")
				:br
				(:input :type "submit" :value "Register")))))))

(defhandler (blog get ("register" token)) (:|html|)
  (let ((user (getredis token *registration-tokens*)))
    (if user
	(reply (cl-who:with-html-output-to-string (var)
		 (:html 
		  (:body (:h1 (cl-who:str (concatenate 'string "Click Here to Complete Registration as" user)))
			 (:form :action "/blog/register/complete" :method "POST"
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
				(:input :type "hidden" :name "user" :value (cl-who:str user))
				(:input :type "hidden" :name "token" :value (cl-who:str token))
				(:input :type "submit" :value "Register"))))))
	(reply "error"))))

(defhandler (blog post ("register" "complete")) (:|content| "application/json")
  (bind-query () ((user "user")
		  (token "token")
		  (password "password")
		  (password2 "password2")
		  (title "title")
		  (subtitle "subtitle"))
    (when (and (equalp (getredis token *registration-tokens*) user)
	       (string= password password2))
      (with-recursive-connection ()
	(redis:red-sadd *users* user)
	(add-password user password)
	(hmsetredis user *settings-ns*
		    "title" title
		    "subtitle" subtitle "display-name" user))
      (reply (format nil "/blog/main/~a" user) :|redirect|))))
      


(defhandler (blog post ("register")) (:|html|)
  (bind-query () ((e-mail "e-mail")
		  (user "user"))
    (setf user (string-downcase (cl-who:escape-string-iso-8859-1 user)))
    (cond 
      ((or (getredis user *password-ns*)
	   (< (length user) 3))
       (reply (cl-who:with-html-output-to-string (var)
		(:html (:body (:B "Name already taken or name must be at least 3 characters")
			      :br (:b (:a :href "/blog/register" "Try Again")))))))
      ((and e-mail user)
       (let ((token (uuid-string)))
	 (setf (getredis token  *registration-tokens*) user)
	 #|(cl-sendmail:with-email (mailstream e-mail :subject "Your Registration")
	     (cl-who:with-html-output (mailstream)
	       (:a :href  (format nil "http://~a/blog/register/~a" *domain-root* token) "Complete Registration")))|#
	 
	 (let* ((link (format nil "http://~a/blog/register/~a" *domain-root* token)) ;;until i have a server with sendmail set up properly.
		(body (cl-who:with-html-output-to-string (mailstream)
			(cl-who:htm
			 (:html 
			  (:body
			   (:a :href link  "Complete Registration")))))))
	 (reply body)))))))

(defhandler (blog get ("login")) (:|html|)
  (reply (cl-who:with-html-output-to-string (stream)
	   (:body 
	    "Login Name"
	    :br
	    (:input :type "text" :id "user" :name "user") :br
	    "Password"
	    :br
	    (:input :type "password" :id "password" :name "password") :br
	    (:input :type "submit" :value "Login" :onclick 
		    (ps:ps-inline (login
				   (ps:chain ($ "input#user")
					     (val))
				   (ps:chain ($ "input#password")
					     (val)))))
	    :br 
	    "Don't Have an Account? " 
	    (named-link stream "Register" "/blog/register/" "div#blog" )
	    ))))






(defhandler (blog post ("login")) (:|content| "application/json")
  (let ((q (parse-query *query*)))
    (let ((user (string-downcase (cl-who:escape-string-iso-8859-1 (second (assoc "user" q  :test #'string=)))))
	  (password (second (assoc "password" q :test #'string=))))
      (let ((uuid (create-login user password)))
	(if uuid 
	    (reply-status "success" 
			  "expires" *expire-days*
			  "cookieId" *site-cookie-name*
			  "postId" (most-recent-post user)
			  "sessionId" uuid)
	    (reply-status "failure"))))))

(defhandler (blog post ("re-auth")) (:|content| "application/json")
  (let ((q (parse-query *query*)))
    (let ((session-id (second (assoc "session-id" q :test #'string=))))
      ;(format t "~s~%" session-id)
      (let ((user (check-login session-id)))
	(if user
	    (reply-status "success" "user" user)
	    (reply-status "failure" "user" ""))))))

(let ((chat-reply-table (make-hash-table :test  #'equalp :synchronized t))
      (lock (bt:make-lock))
      (condition-var (bt:make-condition-variable))
      (reply-count 0)
      (max-queued-replies 500))

  (defun get-chat-text (user &optional (chat-length 20))
    (let ((text-list (lrangeredis user *chat-ns* 0 chat-length)))
      (apply #'concatenate 'string (nreverse text-list))))

  (defun set-chat-text (user string)
    (lpushredis user *chat-ns* string)
    (reply-chat user))

  (defun queue-request (user) 
    (sb-ext:with-locked-hash-table (chat-reply-table)
      (push (get-reply-information) (gethash user chat-reply-table nil))
      (if (>= reply-count max-queued-replies)
	  (bt:with-lock-held (lock)
	    (bt:condition-notify condition-var))
	  (incf reply-count))))

  (defun reply-all-chats ()
    (sb-ext:with-locked-hash-table (chat-reply-table)
      (decf reply-count reply-count) 
      (maphash (lambda (user chat-reply-list)
		 (let ((text (get-chat-text user)))
		   (reply-all text chat-reply-list :|html| nil)))
	       chat-reply-table)))

  (defun init-reply-chat-thread ()
    (bt:make-thread (lambda ()
		      (bt:with-lock-held (lock)
			(do ()
			    (NIL)
			  (bt:condition-wait condition-var lock)
			  (reply-all-chats))))))


  (defun reply-chat (user)
    (let ((chat-reply-list (sb-ext:with-locked-hash-table (chat-reply-table)
			     (prog1 (gethash user chat-reply-table)
			       (setf (gethash user chat-reply-table) nil)))))
      (let ((text (get-chat-text user)))
	(reply-all text  chat-reply-list :|html| nil)))))

(defhandler (blog get ("chat")) (:|html|)
  (reply (cl-who:with-html-output-to-string (val)
	   (:html (:body 
		   (:a :href "#" :onclick (ps:ps-inline (chat-history 0 20)) "history")
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
			   :onclick (ps:ps-inline (post-chat-msg))))))))

(defhandler (blog get ("chat" "wait" author)) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (when (check-login session-id)
      (queue-request author))))

(defhandler (blog get ("chat" "instant" author)) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=))))
    (when (check-login session-id)
      (reply (get-chat-text author)))))

(defhandler (blog get ("chat" "history" author)) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (start (parse-integer (second (assoc "start" q :test #'string=)) :junk-allowed t))
	 (end (parse-integer (second (assoc "end" q :test #'string=)) :junk-allowed t)))
    (if (and (check-login session-id) start end)
	(reply-status 
	 "success"
	 "result"
	 (cl-who:with-html-output-to-string (var)
	   (when (>= start 20) 
	     (cl-who:htm (:a :href "#" :onclick (ps:ps-inline* `(chat-history 
								 ,(- start 20)
								 ,(- end 20))) "Newer")
			 (cl-who:str " ")))
	   (cl-who:htm (:a :href "#" :onclick (ps:ps-inline* `(chat-history
							       ,(+ start 20)
							       ,(+ end 20))) "Older") :br)

	   (cl-who:str (apply #'concatenate 'string (nreverse (lrangeredis author *chat-ns* start end))))))
	(reply-status "failure"))))
			      

(defhandler (blog post ("chat" "p" author)) (:|html|)
  (reply "")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (message (second (assoc "message" q :test #'string=))))
    (let ((name (check-login session-id)))
      (when (and name message)
	(set-chat-text author (cl-who:with-html-output-to-string (str)
				(cl-who:str (timestamp)) " " 
				(:a :href (format nil "/blog/main/~a" name) 
				    (cl-who:str (format nil "~a:" (hgetredis name "display-name" *settings-ns*))))
				" "
				(cl-who:str (cl-who:escape-string-iso-8859-1  message))
				:br))))))

(defun friend-p (user friend)
  (let ((predicated (predicate user *friends-ns*)))
    (with-recursive-connection () (redis:red-sismember predicated friend))))

(defun add-friend (user friend)
  (let ((user-friends (predicate user *friends-ns*))
	(friend-followers (predicate friend *followers-ns*))
	(friend-follower-mailboxes (predicate friend *follower-mailboxes-ns*)))
    (when (with-recursive-connection () 
	    (and (redis:red-sismember *users* friend)
		 (redis:red-sismember *users* user)))
      (with-recursive-connection ()
	(redis:with-pipelining 
	  (redis:red-sadd user-friends friend)
	  (redis:red-sadd friend-followers user)
	  (redis:red-sadd friend-follower-mailboxes (predicate user *mailbox-ns*)))))))

(defun get-friends (user)
  (let ((predicated (predicate user *friends-ns*)))
    (with-recursive-connection ()
      (redis:red-smembers predicated))))

(defhandler (blog post ("friends" "json" "add")) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (user (check-login session-id)))
    (block nil
      (when user
	(let ((friends (second (assoc "friends" q :test #'string=))))
	  (when friends
	    (let ((friends-list (cl-ppcre:split " " friends)))
	      (dolist (friend friends-list)
		(add-friend user friend)))
	    (reply-status "success")
	    (return))))
      (reply-status "failure"))))

(defhandler (blog get ("friends")) (:|html|)
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (user (check-login session-id)))
    (block nil
      (when user
	(let ((user-to-lookup (second (assoc "user" q :test #'string=))))
	  (let ((friends-list (when user-to-lookup
				(get-friends user-to-lookup))))
	    (when friends-list
	      (reply 
	       (cl-who:with-html-output-to-string (var)
		 (dolist (friend friends-list)
		   (cl-who:htm (:a :href (format nil "/blog/main/~a" friend) (cl-who:str friend)) :br))))
	      (return)))))
      (reply ""))))

(defhandler (blog get ("friends" "json" "list")) (:|content| "application/json")
  (let* ((q (parse-query *query*))
	 (session-id (second (assoc "session-id" q :test #'string=)))
	 (user (check-login session-id)))
    (block nil
      (when user
	(let ((user-to-lookup (second (assoc "user" q :test #'string=))))
	  (let ((friends-list (when user-to-lookup
				(get-friends user-to-lookup))))
	    (when friends-list
	      (reply-status "success" "friends" friends-list)
	      (return))))))
    (reply-status "failure")))
	
(generate-appmods)

(defun blog-main ()
  
  ;;can go into redis later on.

  (setf *yaws-server-node-name* "jon-desktop")
  (setf *cookie-file* "/home/jon/github/Lisp-on-Yaws/COOKIE")

  (with-recursive-connection ()
    (setf *salt* (let ((salt (redis:red-get "PASSWORD:SALT")))
		   (if salt 
		       salt
		       (let ((uuid (uuid-string))) 
			 (redis:red-set "PASSWORD:SALT" uuid)
			 uuid))))
    (setf *site-cookie-name* (let ((cookie  (redis:red-get "SITE:COOKIE")))
			       (if cookie 
				   cookie
				   (let ((uuid (uuid-string))) 
				     (redis:red-set "SITE:COOKIE" uuid)
				   uuid))))
    (unless (redis:red-get *post-counter*) (redis:red-set *post-counter* 1)))
  (init-server-connection)
  (init-reply-chat-thread)
  (generate-appmods))