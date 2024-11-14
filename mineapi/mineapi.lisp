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

(defgeneric get-version (downloadable &optional version))

(defmethod get-version ((downloadable downloadable) &optional version)
  (get-latest downloadable))

(defmethod get-version ((downloadable server) &optional version)
  (if (boundp version)
	  ))

(define-json-expander:define-json-expander mine-version-manfest ()
  ((latest
	:json-decoder #'decode-mine-latest)
   (versions
	:json-decoder (lambda (x)
					(loop for obj in x
						  collect (decode-mine-versions obj))))))

(define-json-expander:define-json-expander mine-versions ()
  ((id)
   (type)
   (url)
   (date :json-prop :time)
   (release-time)))

(define-json-expander:define-json-expander mine-latest ()
  ((release)
   (snapshot)))
