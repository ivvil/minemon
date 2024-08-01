;;;; cl-sconf.lisp

(in-package #:dev.shft.cl-sconf)

(defvar *config* nil
  "The global configuration plist.")

(defun read-config (file)
  "Reads the configuration plist from the file FILE"
  (with-open-file (stream file :direction :input)
	(read stream)))

(defun write-config (config file)
  "Write CONFIG (a plist) to FILE in S-expression format."
  (with-open-file (stream file :direction :output :if-exists :supersede)
	(pprint config stream)))

(defun get-config (key &optional default)
  "Get the value associated with KEY in the global configuration plist, or DEFAULT if not found."
  (or (getf *config* key) default))

(defun set-config (key value)
  "Set the value associated with KEY in the global configuration plist to VALUE."
  (setf *config* (plist-put *config* key value)))

(defun plist-put (plist key value)
  "Set the value associated with KEY in PLIST to VALUE."
  (let ((pos (position key plist :test 'equal)))
    (if pos
        (setf (nth (1+ pos) plist) value)
        (setf plist (append plist (list key value)))))
  plist)
