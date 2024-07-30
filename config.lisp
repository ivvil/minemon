;;;; config.lisp

(in-package #:dev.shft.minemon)

(defclass config ()
  ((reserved-ram
	:initarg :reserved-ram
	:initform 0.5
	:accessor reserved-ram)))


