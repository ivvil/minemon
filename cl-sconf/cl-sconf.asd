;;;; cl-sconf.asd

(asdf:defsystem #:cl-sconf
  :description "Common Lisp s-exp based configuration framework"
  :author "Iván Villagrasa <ivvil412@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-sconf")))
