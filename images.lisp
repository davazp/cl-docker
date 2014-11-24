(defpackage :docker/images
  (:use :common-lisp :docker/request)
  (:import-from :yason)
  (:export #:create-image
           #:list-images
           #:inspect-image
           #:image-history))

(in-package :docker/images)

(defun create-image (from-image &key (output *standard-output*) (error *error-output*))
  "Create an image from FROM-IMAGE."
  (multiple-value-bind (stream headers)
      (request (format nil "/images/create~a"
                       (query-string "fromImage" from-image))
               :method :post)
    (declare (ignorable headers))
    (handler-case
        (loop
           for x = (yason:parse stream :object-as :alist)
           do (print x)
           do (let ((message (cdr (assoc "status" x :test #'string=))))
                (when (and message output)
                  (write-line message output)))
           do (let ((message (cdr (assoc "error" x :test #'string=))))
                (when (and message error)
                  (write-line message error))))
      (end-of-file ()))))


(defun list-images (&key all)
  (request-json (format nil "/images/json~:[~;?all=1~]" all)))

(defun inspect-image (name)
  (request-json (format nil "/images/~a/json" name)))

(defun image-history (name)
  (request-json (format nil "/images/~a/history" name)))
