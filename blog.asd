(in-package :asdf)
(defsystem :blog
    :name "blog"
    :depends-on 
    (:cl-who 
     :parenscript
	     :cl-ppcre
	     :cl-markdown
	     :cl-markdown
	     :uuid
	     :cl-json
	     :cl-sendmail
	     :cl-redis
	     :sb-concurrency
	     :lisp-on-yaws)
    :version "0.0.1"
    :components ((:file "blog"
			:depends-on ("package" "query" :utils))
		 (:file "query"
			:depends-on ("package" :utils))
		 (:module :utils
			  :components ((:file "munge")
				       (:file "ps")
				       (:file "redis"))
			  :depends-on ("package"))
		 (:file "package")))