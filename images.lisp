(defpackage :docker/images
  (:use :common-lisp :docker/request)
  (:export #:create-image
           #:list-images
           #:inspect-image
           #:image-history))

(in-package :docker/images)

(defun create-image (from-image)
  (request-json (format nil "/images/create~a"
                        (query-string "fromImage" from-image))
                :method :post))


(defun list-images (&key all)
  (request-json (format nil "/images/json~:[~;?all=1~]" all)))

(defun inspect-image (name)
  (request-json (format nil "/images/~a/json" name)))

(defun image-history (name)
  (request-json (format nil "/images/~a/history" name)))
