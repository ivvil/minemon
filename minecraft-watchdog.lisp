;;;; minecraft-watchdog.lisp

(in-package #:dev.shft.minemon)

(defmacro minecraft-invocation (java-path minecraft-jar &optional options)
  `(list ,java-path "-jar" ,minecraft-jar ,(or options "")))

