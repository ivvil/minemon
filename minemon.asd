;;;; minemon.asd

(asdf:defsystem #:minemon
  :description "A minecraft server management and monitoring suite"
  :author "Iván Villagrasa <ivvil412@gmail.com>"
  :license  "GPL-V3"
  :version "0.0.1"
  :serial t
  :depends-on (:bt-semaphore)
  :components ((:file "package")
               (:file "minemon")))