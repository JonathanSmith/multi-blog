(in-package :blog)

(defvar *users* "USERS")
(defvar *password-ns* "PW")
(defvar *salt-ns* "SALT")
(defvar *settings-ns* "SET")
(defvar *pst-ns* "PST")
(defvar *pst-title* "PST-TITLE")
(defvar *pst-idx* "PSTDX")
(defvar *pst-res* "PSTRE")
(defvar *login-cookie-ns* "LC")

(defvar *friends-ns* "FRND")
(defvar *followers-ns* "FOLW")
(defvar *mailbox-ns* "MLBX")
(defvar *follower-mailboxes-ns* "FMBX")
(defvar *post-counter* "PST-CNT")
(defvar *chat-counter* "CHT-CNT")
(defvar *chat-ns* "CHAT")
(defvar *chat-info* "CHAT-INFO")
(defvar *chats-owned* "CHTDCT")
(defvar *chat-subs* "CHTIDX")
(defvar *registration-tokens* "REGTKN")

(defvar *expire-days* 1)
(defvar *login-timeout* (* *expire-days* 24 60 60))

(defun generate-post-html (post-id universal-time author title body)
  (multiple-value-bind (second minute hour date month year)  (decode-universal-time universal-time)
    (declare (ignore second))
    (let* ((body-string (let ((s (make-string-output-stream))) 
			  (cl-markdown:markdown body :stream s)
			  (get-output-stream-string s)))
	   (date-string (format nil "At ~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d" year month date hour minute))
	   (page (cl-who:with-html-output-to-string (var)
		   (:h2 
		    (:a :href 
			(format nil "/blog/viewpost/~a" post-id)
			(cl-who:str title)))
		   (cl-who:str body-string)
		   (:a :href (cl-who:str (format nil "/blog/main/~a" author))
		       (:h4 (cl-who:str (hgetredis author "display-name" *settings-ns*))))
		   (:h4 (cl-who:str date-string))
		   
		   )))
      page)))

(defun add-post-reply (post-id reply-id)
  (with-recursive-connection ()
    (when (and (hgetredis post-id "author" *pst-ns*)
	       (hgetredis reply-id "author" *pst-ns*))
      (redis:with-pipelining 
	(hsetredis reply-id "reply-to" post-id *pst-ns*)
	(redis:red-zadd (predicate post-id *pst-res*) reply-id reply-id)))))

(defun remove-reply-post (reply-id)
  (with-recursive-connection ()
    (let ((in-reply-to (hgetredis reply-id "reply-to" *pst-ns*)))
      (when in-reply-to
	(redis:red-zrem (predicate in-reply-to *pst-res*) reply-id)))))


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
	  (redis:red-sadd friend-follower-mailboxes (predicate user *mailbox-ns*)))
	(add-user-posts-to-mailbox user friend)))))

(defun add-user-posts-to-mailbox (user friend)
  (let ((mailbox (predicate user *mailbox-ns*)))
    (with-recursive-connection () 
      (let ((posts (redis:red-zrange (predicate friend *pst-idx*) 0 -1)))
	(redis:with-pipelining 
	  (map nil (lambda (post) (redis:red-zadd mailbox post post)) posts))))))

(defun remove-friend (user friend)
  (let ((user-friends (predicate user *friends-ns*))
	(friend-followers (predicate friend *followers-ns*))
	(friend-follower-mailboxes (predicate friend *follower-mailboxes-ns*)))
    (with-recursive-connection ()
	(when (and (redis:red-sismember *users* friend)
		   (redis:red-sismember *users* user))
	  (redis:with-pipelining
	    (redis:red-srem user-friends friend)
	    (redis:red-srem friend-followers user)
	    (redis:red-srem friend-follower-mailboxes (predicate user *mailbox-ns*)))
	  (remove-user-posts-from-mailbox user friend)))))

(defun remove-user-posts-from-mailbox (user friend)
  (let ((mailbox (predicate user *mailbox-ns*)))
    (with-recursive-connection () 
      (let ((posts (redis:red-zrange (predicate friend *pst-idx*) 0 -1)))
	(redis:with-pipelining 
	  (map nil (lambda (post) (redis:red-zrem mailbox post)) posts))))))


(defun generate-post-from-db (pst-id)
    (let ((pst-props (hmgetredis pst-id *pst-ns*)))
      (if pst-props
	  (destructure-props ((universal-time "time")
			      (author "author")
			      (title "title")
			      (body "body"))
	      pst-props
	    (let ((time (safe-parse-int universal-time)))
	      (generate-post-html pst-id time author title body)))
	  "")))

(defun get-friends (user)
  (let ((predicated (predicate user *friends-ns*)))
    (with-recursive-connection ()
      (redis:red-smembers predicated))))

(defun get-followers (user)
  (let ((predicated (predicate user *followers-ns*)))
    (with-recursive-connection ()
      (redis:red-smembers predicated))))

(defun generate-post-entry (title author lines)
  (let* ((time (get-universal-time))
	 (post-id (with-recursive-connection ()
		    (first (fourth (redis:with-pipelining 
				     (with-transaction 
				       (redis:red-get *post-counter*)
				       (redis:red-incr *post-counter*))))))))
    (unless post-id
      (with-recursive-connection ()
	(setf post-id 
	      (first (fourth (redis:with-pipelining 
			       (with-transaction 
				 (redis:red-get *post-counter*)
				 (redis:red-incr *post-counter*))))))))
    (with-recursive-connection ()
      (redis:with-pipelining
	(redis:red-zadd (predicate author *pst-idx*) post-id post-id)
	(redis::red-hmset (predicate post-id *pst-ns*)
			  "title" title
			  "author" author
			  "time" time
			  "body" lines))
      (add-post-to-follower-mailboxes author post-id))
    post-id))

(defun generate-chat-entry (title owner)
  (let* ((time (get-universal-time))
	 (chat-id (with-recursive-connection ()
		    (first (fourth (redis:with-pipelining 
				     (with-transaction 
				       (redis:red-get *chat-counter*)
				       (redis:red-incr *chat-counter*))))))))
    (unless chat-id
      (with-recursive-connection ()
	(setf chat-id 
	      (first (fourth (redis:with-pipelining 
			       (with-transaction 
				 (redis:red-get *chat-counter*)
				 (redis:red-incr *chat-counter*))))))))
    (with-recursive-connection ()
      (redis:with-pipelining
	(lpushredis owner *chat-subs* chat-id)
	(lpushredis owner *chats-owned* chat-id)
	(hmsetredis chat-id *chat-info* 
		    "title" title
		    "owner" owner
		    "time" time)))
    chat-id))

(defun subscribe-to-chat (user chat-id)
  (lpushredis user *chat-subs* chat-id))

(defun chat-subs (user)
  (let ((chat-ids (lrangeredis user *chat-subs* 0 -1)))
    (with-recursive-connection ()
      (let ((titles (redis:with-pipelining  
		      (map nil (lambda (chat-id) (hgetredis chat-id "title" *chat-info*)) chat-ids)))
	    (owners (redis:with-pipelining
		      (map nil (lambda (chat-id) (hgetredis chat-id "owner" *chat-info*)) chat-ids))))
	(values chat-ids titles owners)))))

(defun chat-owned-p (user chat)
  (declare (ignore user chat))
  t)

(defun owned-chats (user)
  (let ((chat-ids (lrangeredis user *chats-owned* 0 -1)))
    (with-recursive-connection ()
      (let ((titles (with-transaction
		      (map nil (lambda (chat-id) (hgetredis chat-id "title" *chat-info*)) chat-ids))))
	(values chat-ids titles)))))

(defun chat-owner-p (user chat-id)
  (equalp user (with-recursive-connection ()
		 (hgetredis chat-id "owner" *chat-info*))))


(defun remove-post-entry (author post-id)
  (with-recursive-connection ()
    (remove-reply-post post-id)
    (redis:red-zrem (predicate author *pst-idx*) post-id)
    (redis:red-del (predicate post-id *pst-ns*))
    (remove-post-from-follower-mailboxes author post-id)))

(defun update-post-entry (post-id title author lines)
  (let* ((time (get-universal-time)))
    (redis:red-hmset (predicate post-id *pst-ns*) 
		     "title" title
		     "author" author
		     "time" time
		     "body" lines)))

(defun add-post-to-follower-mailboxes (author post-id)
  (with-recursive-connection ()
    (let ((mailboxes (redis:red-smembers (predicate author *follower-mailboxes-ns*))))
      (redis:with-pipelining 
	(dolist (mailbox mailboxes)
	  (redis:red-zadd mailbox post-id post-id))))))

(defun remove-post-from-follower-mailboxes (author post-id)
  (with-recursive-connection ()
    (let ((mailboxes (redis:red-smembers (predicate author *follower-mailboxes-ns*))))
      (redis:with-pipelining 
	(dolist (mailbox mailboxes)
	  (redis:red-zrem mailbox post-id))))))

(defun most-recent-post (author)
  (first (with-recursive-connection ()
	   (redis:red-zrevrange (predicate author *pst-idx*) 0 0))))

(defun generate-index (author &key (start 0) (end -1))
  (let ((post-index (with-recursive-connection ()
		      (redis:red-zrevrange (predicate author *pst-idx*)  start end))))
    (cl-who:with-html-output-to-string (var)
      (:ul :class "navbar"
	   (loop for id in post-index
	      do 
	      (let ((link (format nil "/blog/viewpost/~a" id)))
		(clickable-li var (hgetredis id "title" *pst-ns*)  link "div#blog" `(lambda () 
										      (setf *post-id* ,id)
										      (update-history)
										      (post-process)) '(session-obj)))))
      (:input :type "hidden" :id "latest" :name "latest" :value (most-recent-post author)))))

(defun add-password (name password)
  (setf (getredis (string-downcase name) *password-ns*) (bcrypt:hash password)))

(defun check-password (name password)
  (let ((name (string-downcase name)))
    (bcrypt:password= password (getredis name *password-ns*))))

(defun create-login (user password)
  (when (check-password user password)
    (let* ((uuid (uuid-string)))
      (setf (getredis uuid *login-cookie-ns* *login-timeout*) (string-downcase user))
      uuid)))

(defun timeout-session (uuid)
  (when (check-login uuid)
    (with-recursive-connection () 
      (redis:red-del (predicate uuid *login-cookie-ns*)))))

(defun check-login (uuid)
  (let ((user (getredis uuid *login-cookie-ns*)))
    (when user
      (setf (getredis uuid *login-cookie-ns* *login-timeout*) user) user)))