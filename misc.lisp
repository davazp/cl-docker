(defpackage :docker/misc
  (:use :common-lisp :docker/request)
  (:import-from :uiop #:copy-stream-to-stream)
  (:export #:info
           #:version
           #:ping))

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
