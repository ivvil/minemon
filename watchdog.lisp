;;;; watchdog.lisp

(in-package #:dev.shft.minemon)

(defparameter *watched-instances* '())

(defclass minecraft-watchdog ()
  ((instance
	:initarg :instance
	:initform (error "An instance must be provided for watchdog creation")
	:accessor instance)
   (restarts
	:initform 0
	:accessor restarts)
   (time-of-last-restart
	:initform (get-universal-time)
	:accessor last-restart-time)
   (process
	:initform nil
	:accessor process)))

(defmethod m-equal ((x minecraft-watchdog) (y minecraft-watchdog))
  (m-equal (instance x) (instance y)))

(defmethod run ((instance minecraft-watchdog))
  (let ((process (run (instance instance))))
	(setf (process instance) process)
	(pushnew instance *watched-instances* :test 'm-equal)))

(defun restart-instance (instance)
  (incf (restarts instance))
  (setf (last-restart-time instance) (get-universal-time))
  (run instance))

(defun restart-stopped-instances (instances)
  (loop for instance in instances
		if (process-alive-p (process instance))
		  do (restart-instance instance)))
(defun watch-instance (instance)
  (make-instance 'minecraft-watchdog :instance instance))




