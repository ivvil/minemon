;;;; mineapi.lisp

(in-package #:dev.shft.mineapi)

(defclass downloadable ()
  ((display-name
	:reader display-name
	:initarg :name)
   (url
	:accessor url
	:initarg :url)))

(defclass versionable () ())

(defclass server (downloadable versionable) ())

(defgeneric get-versions (server))

(defmethod get-versions ((server server))
  (let ((manifest (cl-json:decode-json-from-string (dex:get (url server)))))
	(slot-value (decode-mine-version-manfest manifest) 'versions)))

(defgeneric get-latest (downloadable))

(defmethod get-latest ((downloadable downloadable))
  (url downloadable))

(defmethod get-latest ((downloadable server))
  (let ((manifest (cl-json:decode-json-from-string (dex:get (url downloadable)))))
	(slot-value (decode-mine-version-manfest manifest) 'latest)))

(defgeneric get-version (downloadable version))

(defmethod get-version ((downloadable server) version)
  (let ((versions (get-versions downloadable)))
	(gethash version versions)))

(define-json-expander:define-json-expander mine-version-manfest ()
  ((latest
	:json-decoder #'decode-mine-latest)
   (versions
	:json-decoder (lambda (x)
					(let ((version-table (make-hash-table :test 'equal)))					  
					  (loop for obj in x
							for version = (decode-mine-versions obj)
							do (setf (gethash (slot-value version 'id) version-table) version))
					  version-table)))))

(define-json-expander:define-json-expander mine-versions ()
  ((id)
   (type)
   (url)
   (date :json-prop :time)
   (release-time)))

(define-json-expander:define-json-expander mine-latest ()
  ((release)
   (snapshot)))
