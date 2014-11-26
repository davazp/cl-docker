(defpackage :docker/misc
  (:use :common-lisp :docker/request)
  (:import-from :uiop #:copy-stream-to-stream)
  (:export #:info
           #:version))

(in-package :docker/misc)

(defun info ()
  (request-json "/info"))

(defun version ()
  (request-json "/version"))
