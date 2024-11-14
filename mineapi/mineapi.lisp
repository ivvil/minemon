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
	(decode-mine-version-manfest manifest)))

(define-json-expander:define-json-expander mine-version-manfest ()
  ((versions
	:json-decoder (lambda (x)
					(loop for obj in x
						  collect (decode-mine-versions obj))))))

(define-json-expander:define-json-expander mine-versions ()
  ((id)
   (type)
   (url)
   (date :json-prop :time)
   (release-time)))
