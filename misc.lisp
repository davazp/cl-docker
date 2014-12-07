(defpackage :docker/misc
  (:use :common-lisp :docker/request)
  (:import-from :uiop #:copy-stream-to-stream)
  (:import-from #:yason)
  (:export #:info
           #:version
	   #:ping
	   #:monitor-events))

(in-package :docker/misc)

(defun info ()
  (request-json "/info"))

(defun version ()
  (request-json "/version"))

(defun ping ()
  (with-output-to-string (out)
    (with-open-stream (stream (request "/_ping"))
      (loop
	 (multiple-value-bind (line lastp)
	     (read-line stream nil)
	   (cond
	     (lastp
	      (write-string line out)
	      (return))
	     (t
	      (write-line line out))))))))


(defun monitor-events ()
  (with-open-stream (stream (request "/events"))
    ;; TODO: Invoke a callback instead (or in addition to) print it.
    (loop
       for event = (yason:parse stream :object-as :plist)
       do (print event))))
