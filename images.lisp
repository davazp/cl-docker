(defpackage :docker/images
  (:use :common-lisp :docker/request)
  (:import-from :yason)
  (:export #:create-image
           #:list-images
           #:inspect-image
           #:image-history
           #:tag-image
           #:remove-image))

(in-package :docker/images)


(defun parse-repository-tag (string)
  "Parse a image name and return two values. The first one is the
repository and the second one is the tag. If the string does not
contains a tag, the second value is NIL."
  (declare (string string))
  (let ((i (position #\: string :from-end t)))
    (if (null i)
        (values string nil)
        (let ((part1 (subseq string 0 i))
              (part2 (subseq string (1+ i))))
          (if (find #\/ part2)
              (values string nil)
              (values part1 part2))))))


(defun list-images (&key all filters)
  ;; filters example: {"dangling": ["true"]}
  (request-json (format nil "/images/json~a"
                        (query-string
                         "all" (and all 1)
                         "filters" (url-encode filters)))))


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



(defun inspect-image (name)
  (request-json (format nil "/images/~a/json" name)))

(defun image-history (name)
  (request-json (format nil "/images/~a/history" name)))


(defun tag-image (name tag &key repo force)
  (declare (string tag))
  (request-json (format nil "/images/~a/tag~a"
                        (url-encode name)
                        (query-string
                         "tag" (url-encode tag)
                         "repo" (and repo (url-encode repo))
                         "force" (and force 1)))
                :method :post))


(defun remove-image (name &key force noprune)
  (request-json (format nil "/images/~a~a"
                        (url-encode name)
                        (query-string
                         "force" (and force 1)
                         "noprune" (and noprune 1)))
                :method :delete))
