;;;; utils.lisp

(in-package #:dev.shft.minemon)

(defun create-temporary-dir ()
  (ensure-directories-exist (merge-pathnames (temporary-directory) )))

(defun gen-random-pathname ()
  (make-pathname :directory ))
