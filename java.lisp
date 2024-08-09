;;;; java.lisp

(in-package #:dev.shft.minemon)

(defclass java-install ()
  ((name
	:initarg :name
	:initform "java"
	:accessor name)
   (arch
	:initform "unknown"
	:accessor arch)
   (path
	:initarg :path
	:accessor path)))

(defgeneric java-version (java))

(defmethod java-version ((java string))
  (if (string-prefix-p "1.")))

(defmethod java-version ((java java-install))
  )

(defun java-paths-linux ()
  )

(defun default-java ()
  )
