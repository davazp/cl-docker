#-asdf3 (error "docker requires ASDF 3")

(asdf:defsystem :docker
  :name "Common Lisp Docker Library"
  :description "A Common Lisp client library for the Docker Remote API"
  :version "0.0.1"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:docker/all))
