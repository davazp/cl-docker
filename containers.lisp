(defpackage :docker/containers
  (:use :common-lisp :docker/request)
  (:export #:list-containers
           #:inspect-container
           #:list-container-processes
           #:inspect-container-changes
           #:remove-container))

(in-package :docker/containers)

(defun list-containers (&key limit since before size all)
  (let ((url (format nil "/containers/json~@[?limit=~d~]~@[?since=~a~]~@[?before=~d~]~:[~;?size=1~]~:[~;?all=1~]"
                     limit since before size all)))
    (request-json url)))


(defun inspect-container (id)
  (request-json (format nil "/containers/~a/json" id)))

(defun list-container-processes (id)
  (request-json (format nil "/containers/~a/top" id)))

(defun inspect-container-changes (id)
  (request-json (format nil "/containers/~a/changes" id)))

(defun remove-container (id)
  (request-json (format nil "/containers/~a" id) :method :delete))
