
(defsystem :cl-docker
  :name "Common Lisp Docker Library"
  :description "A Common Lisp client library for the Docker Remote API"
  :version "0.0.1"
  :license "MIT"
  :depends-on (:flexi-streams :chunga :yason)
  :components
  ((:file "docker")))
