(in-package :blog)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *redis-connection-queue* (sb-concurrency::make-queue))
  (defmacro with-recursive-connection ((&key (host #(127 0 0 1))
					     (port 6379))
				       &body body)
    #|`(redis:with-recursive-connection (:host ,host :port ,port)
    ,@body)|#
    
    #|`(bt:with-recursive-lock-held (*redis-mutex*)
    (redis:with-recursive-connection ,connection-spec ,@body)))|#

    (let ((connection (gensym)))
      `(if (redis::connected-p)
	     (progn
	       ;(format t "recursive: ~s~%" redis::*connection*)
	       ,@body)
	     (let ((,connection (sb-concurrency:dequeue blog::*redis-connection-queue*)))
	       (if (and ,connection (redis::open-connection-p ,connection))
		   (let ((redis::*connection* ,connection))
		;     (format t "reuse:~s~%"  redis::*connection*)
		     (unwind-protect (progn ,@body)
		       (sb-concurrency::enqueue redis::*connection* blog::*redis-connection-queue*)))
		   (let ((redis::*connection* (make-instance 'redis::redis-connection
							     :host ,host
							     :port ,port)))
		 ;    (format t "new:~s~%" redis::*connection*)
		     (unwind-protect (progn ,@body)
		       (sb-concurrency::enqueue redis::*connection* blog::*redis-connection-queue*))))))))

(defun close-all-redis-connections ()
    (do  ((connection (sb-concurrency::dequeue *redis-connection-queue*) (sb-concurrency:dequeue *redis-connection-queue*)))
	 ((not connection))
      (redis::close-connection connection)))

  (defmacro with-transaction (&body body)
    (let ((completed (gensym)))
      `(let ((,completed nil))
	 (unwind-protect
	      (progn 
		(redis:red-multi)
		,@body
		(redis:red-exec)
		(setf ,completed t))
	   (unless ,completed (redis:red-discard)))))))

(defun predicate (key ns) (concatenate 'string ns ":" key))

(defun setredis (key ns val &optional secs)
  (let ((predicated (predicate key ns)))
    (if secs
	(with-recursive-connection ()
	  (first (redis:with-pipelining 
		   (redis:red-set predicated val)
		   (redis:red-expire predicated secs))))
	(with-recursive-connection ()
	  (redis:red-set predicated val)))))



(defun getredis (key ns)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
		       (redis:red-get predicated))))

(defun hgetredis (key field ns)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
      (redis:red-hget predicated field))))

(defun hsetredis (key field value ns)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
      (redis:red-hset predicated field value))))

(defun hmgetredis (key ns)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
      (redis:red-hgetall predicated))))

(defun hmsetredis (key ns &rest vals)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
      (apply #'redis:red-hmset predicated vals))))
 

(defun lpushredis (key ns val)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
      (redis:red-lpush predicated val))))

(defun lrangeredis (key ns start end)
  (let ((predicated (predicate key ns)))
    (with-recursive-connection ()
      (redis:red-lrange predicated start end))))

(defsetf getredis (key ns &optional expire) (store)
  (if expire
      `(setredis ,key ,ns ,store ,expire)
      `(setredis ,key ,ns ,store)))