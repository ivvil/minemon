;;;; minecraft-watchdog.lisp

(in-package #:dev.shft.minemon)

(defmacro minecraft-invocation (java-path minecraft-jar &optional options)
  `(list ,java-path "-jar" ,minecraft-jar ,(or options "")))

(defun launch-minecraft (java-path minecraft-jar &optional options)
  (uiop:launch-program (minecraft-invocation java-path minecraft-jar options)
					   :output :stream
					   :input :stream))


