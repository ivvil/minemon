;;;; minemon.asd

(asdf:defsystem #:minemon
  :description "A minecraft server management and monitoring suite"
  :author "Iván Villagrasa <ivvil412@gmail.com>"
  :license  "GPL-V3"
  :version "0.0.1"
  :serial t
  :depends-on (:bt-semaphore
			   :machine-state			;Needs shirakumo dist installed
			   :uiop
			   :cxml
			   :drakma
			   :parse-float
			   :split-sequence)						
  :components ((:file "package")
               (:file "minemon")
			   (:file "config")
			   (:file "system")
			   (:file "watchdog")
			   (:file "java")
			   (:file "instance")))
