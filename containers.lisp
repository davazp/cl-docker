(defpackage :docker/containers
  (:use :common-lisp :docker/request)
  (:export #:list-containers
           #:inspect-container
           #:list-container-processes
           #:inspect-container-changes
           #:remove-container))

(in-package :docker/containers)

(defun list-containers (&key limit since before size all)
  (let ((url (format nil "/containers/json~a"
                     (query-string "limit" limit
                                   "since" since
                                   "before" before
                                   "size" (and size 1)
                                   "all" (and all 1)))))
    (request-json url)))


(defun inspect-container (id)
  (request-json (format nil "/containers/~a/json" id)))

(defun list-container-processes (id)
  (request-json (format nil "/containers/~a/top" id)))

(defun inspect-container-changes (id)
  (request-json (format nil "/containers/~a/changes" id)))

(defun remove-container (id)
  (request-json (format nil "/containers/~a" id) :method :delete))
