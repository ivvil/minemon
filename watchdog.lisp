;;;; watchdog.lisp

(in-package #:dev.shft.minemon)

(defparameter *watched-instances* '())
(defparameter *restart-warn-amount* 20)
(defparameter *restart-warn-time-interval* 60)

(defclass minecraft-watchdog ()
  ((instance
	:initarg :instance
	:initform (error "An instance must be provided for watchdog creation")
	:accessor instance)
   (restarts
	:initform 0
	:accessor restarts)
   (time-of-last-restart
	:initform (get-universal-time) ; TODO Set this when the watchdog runs
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
  (log:info "Restarting instance " instance)
  (when (warn-restartsp (restarts instance) (last-restart-time instance))
	(log:warn "Multiple restarts on instance " instance "time of last restart -A already restarted ~A" (print-time (last-restart-time instance)) (restarts instance)))
  (incf (restarts instance))
  (setf (last-restart-time instance) (get-universal-time))
  (run instance)) ; NOTE Are we stopping it?

(defun warn-restartsp (restarts last-restart-time)
  (or (< *restart-warn-amount* restarts)
      (> (- last-restart-time
			(get-universal-time))
		 *restart-warn-time-interval*)))

(defun restart-stopped-instances (instances)
  (loop for instance in instances
		if (process-alive-p (process instance))
		  do (restart-instance instance)))

(defun watch-instance (instance)
  (make-instance 'minecraft-watchdog :instance instance))




