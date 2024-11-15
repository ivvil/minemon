;;;; mineapi.asd

(asdf:defsystem #:mineapi
  :license "GPL-v3"
  :version "0.0.1"
  :serial t
  :depends-on (:dexador
			   :cl-json
			   :define-json-expander
			   :trivial-download)
  :components ((:file "package")
			   (:file "mineapi")))
