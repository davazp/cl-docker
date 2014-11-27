(defpackage :docker/errors
  (:use :common-lisp)
  (:export #:docker-condition
	   #:docker-error
	   #:docker-connection-error))

(in-package :docker/errors)


(define-condition docker-condition ()
  nil)

(define-condition docker-error (docker-condition error)
  nil)

(define-condition docker-connection-error (docker-error)
  ((pathname :initarg :pathname))
  (:report
   (lambda (condition stream)
     (format stream "Could not connect to Docker daemon: unknown file ~S."
	     (slot-value condition 'pathname)))))
