(defpackage :docker/images
  (:use :common-lisp :docker/request)
  (:export #:list-images))

(in-package :docker/images)

(defun list-images (&optional all)
  (request-json (format nil "/images/json~@[?all=1~]" all)))

