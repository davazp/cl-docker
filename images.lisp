(defpackage :docker/images
  (:use :common-lisp :docker/request)
  (:import-from :yason)
  (:import-from :uiop #:copy-stream-to-stream)
  (:export #:create-image
           #:list-images
           #:inspect-image
           #:image-history
           #:tag-image
           #:remove-image
           #:search-image
           #:export-repository-to-stream
           #:export-repository-to-pathname
           #:export-all-images-to-stream
           #:export-all-images-to-pathname))

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
  (let ((parameters `(("all" . ,(and all 1))
                      ("filters" . ,(and filters (url-encode filters))))))
    (request-json "/images/json" :parameters parameters)))


(defun create-image (from-image &key (output *standard-output*) (error *error-output*))
  "Create an image from FROM-IMAGE."
  (multiple-value-bind (stream headers)

      (request "/images/create"
               :method :post
               :parameters `(("fromImage" . ,from-image)))
    
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
  (multiple-value-bind (stream)
      (request (format nil "/images/~a/tag" (url-encode name))
               :method :post
               :parameters `(("tag" . ,tag)
                             ("repo" . ,repo)
                             ("force" . ,(and force 1))))
    (close stream)))


(defun remove-image (name &key force noprune)
  (request-json (format nil "/images/~a" (url-encode name))
                :method :delete
                :parameters `(("force" . ,(and force 1))
                              ("noproune" . ,(and noprune 1)))))


(defun search-image (term)
  (declare (string term))
  (request-json "/images/search" :parameters `(("term" . ,term))))



(defun export-repository-to-stream (name stream)
  "Export the repository NAME as a tar archive to STREAM."
  (with-open-stream (tar (request (format nil "/images/~a/get" name)))
    (copy-stream-to-stream tar stream :element-type '(unsigned-byte 8))))

(defun export-repository-to-pathname (name pathname &rest args &key &allow-other-keys)
  "Export the repository NAME as a tar archive to PATHNAME. Keyword
arguments are passed to the function OPEN."
  (with-open-stream (out (apply #'open pathname
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                args))
    (export-repository-to-stream name out)))



(defun export-all-images-to-stream (stream &key names)
  (let ((url (format nil "/images/get~@[?~{names=~a~^&~}~]" names)))
    (with-open-stream (tar (request url))
      (copy-stream-to-stream tar stream :element-type '(unsigned-byte 8)))))

(defun export-all-images-to-pathname (pathname &rest args &key names &allow-other-keys)
  (let ((args (remf args :names)))
    (with-open-stream (out (apply #'open pathname
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  args))
      (export-all-images-to-stream out :names names))))
