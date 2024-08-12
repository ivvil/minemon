;;;; watchdog.lisp

(in-package #:dev.shft.minemon)

(defparameter *watched-instances* '())

(defclass minecraft-watchdog ()
  ((instance
	:initarg :name
	:initform (error "An instance must be provided for watchdog creation")
	:accessor instance)
   (restarts
	:initform 0
	:accessor restarts)
   (time-of-last-restart
	:initform (get-universal-time))
   (process
	:initform nil
	:accessor process)))

(defmethod m-equal ((x minecraft-watchdog) (y minecraft-watchdog))
  (m-equal (instance x) (instance y)))

(defmethod run ((instance minecraft-watchdog))
  (pushnew instance *watched-instances* :test 'm-equal))

