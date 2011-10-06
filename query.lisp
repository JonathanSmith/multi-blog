(in-package :blog)

(defvar *users* "USERS")
(defvar *password-ns* "PW")
(defvar *settings-ns* "SET")
(defvar *pst-ns* "PST")
(defvar *pst-title* "PST-TITLE")
(defvar *pst-idx* "PSTDX")
(defvar *pst-dict* "PSTDCT")
(defvar *login-cookie-ns* "LC")
(defvar *chat-ns* "CHAT")
(defvar *friends-ns* "FRND")
(defvar *followers-ns* "FOLW")
(defvar *mailbox-ns* "MLBX")
(defvar *follower-mailboxes-ns* "FMBX")
(defvar *post-counter* "PST-CNT")
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
		   (:h4 (cl-who:str date-string)))))
      page)))


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

(defun generate-post-entry (title author lines)
  (let* ((time (get-universal-time))
	 (post-id (with-recursive-connection ()
		    (first (fourth (redis:with-pipelining 
				     (with-transaction 
				       (redis:red-get *post-counter*)
				       (redis:red-incr *post-counter*))))))))
    (with-recursive-connection ()
      (redis:with-pipelining
	(saddredis author *pst-dict* post-id)
	(lpushredis author *pst-idx* post-id)
	(hmsetredis post-id *pst-ns* 
		    "title" title
		    "author" author
		    "time" time
		    "body" lines))
      (add-post-to-follower-mailboxes author post-id))
    post-id))

(defun update-post-entry (post-id title author lines)
  (let* ((time (get-universal-time)))
    (hmsetredis post-id *pst-ns* 
		"title" title
		"author" author
		"time" time
		"body" lines)))

(defun add-post-to-follower-mailboxes (author post-id)
  (with-recursive-connection ()
    (let ((mailboxes (redis:red-smembers (predicate author *follower-mailboxes-ns*))))
      (redis:with-pipelining 
	(dolist (mailbox mailboxes)
	  (redis:red-lpush mailbox post-id))))))

(defun most-recent-post (author)
  (first (lrangeredis author *pst-idx* 0 0)))

(defun generate-index (author &key (start 0) (end -1))
  (let ((post-index (lrangeredis author *pst-idx*  start end)))
    (cl-who:with-html-output-to-string (var)
      (:ul :class "navbar"
	   (loop for id in post-index
	      do 
	      (let ((link (format nil "/blog/viewpost/~a" id)))
		(clickable-li var (hgetredis id "title" *pst-ns*)  link "div#blog"))))
      (:input :type "hidden" :id "latest" :name "latest" :value (most-recent-post author)))))



(defun add-password (name password)
    (setf (getredis (string-downcase name) *password-ns*) (obfuscate-password password)))

(defun check-password (name password)
  (string= (getredis (string-downcase name) *password-ns*) (obfuscate-password password)))

(defun create-login (user password)
  (when (check-password user password)
    (let* ((uuid (uuid-string)))
      (setf (getredis uuid *login-cookie-ns* *login-timeout*) (string-downcase user))
      uuid)))

(defun check-login (uuid)
  (let ((user (getredis uuid *login-cookie-ns*)))
    (when user
      (setf (getredis uuid *login-cookie-ns* *login-timeout*) user) user)))