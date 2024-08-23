;;;; instance.lisp

(in-package #:dev.shft.minemon)

(defclass minecraft-instance ()
  ((name
    :initarg :name
	:initform (error "An instance name must be provided for instance creation")
    :accessor name)
   (base-dir
    :initarg :base-dir
    :accessor base-dir)
   (main-jar
    :initarg :jar
    :initform "minecraft.jar"
    :accessor jar)
   (memory
	:initarg :ram
	:initform (recommended-memory-size)
	:accessor memory)
   (extra-args
	:initarg :args
	:initform ""
	:accessor args)
   (java
    :initarg :java
    :initform (default-java)
    :accessor java)))

(defgeneric run (instance))

(defmethod run ((instance minecraft-instance))
  (let ((jar-path (merge-pathnames (jar instance) (base-dir instance))))
	(run-minecraft (java instance) jar-path :memory (memory instance) :args (args instance))))

(defun run-minecraft (java minecraft-jar &key (memory 0 memory-p) (args ""))
  (let* ((memory (princ-to-string memory))
		 (memarg (if memory-p
					 (concatenate 'string "-Xmx " memory "M " "-Xms " memory "M")
					 ""))
		 (javarg (concatenate 'string memarg " " args)))
	(launch-program `(,java
					  "-jar "
					  ,(princ-to-string minecraft-jar) " "
					  ,javarg)
					:output :stream)))

(defmethod m-equal ((x minecraft-instance) (y minecraft-instance))
  (string= (name x) (name y)))

