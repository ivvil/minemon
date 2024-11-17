;;;; mineapi.lisp

(in-package #:dev.shft.mineapi)

(defclass downloadable ()
  ((display-name
	:reader name
	:initarg :name)
   (url
	:accessor url
	:initarg :url)))

(defclass versionable () ())

(defclass game-version (downloadable versionable) ())

(defclass loader-version (downloadable versionable) ())

(defclass version-manifest ()
  ((versions
	:initarg :versions
	:accessor versions)))

(defgeneric get-versions (game-version))

(defmethod get-versions ((game-version game-version))
  (let ((manifest (json:decode-json-from-string (dex:get (url game-version)))))
	(versions (decode-mine-version-manfest manifest))))

(defgeneric get-latest (downloadable))  ;TODO: Think if this function is needed

(defmethod get-latest ((downloadable downloadable))
  (url downloadable))

(defmethod get-latest ((downloadable game-version))
  (let ((manifest (json:decode-json-from-string (dex:get (url downloadable)))))
	(latest (decode-mine-version-manfest manifest))))

(defgeneric get-version (downloadable version))

(defmethod get-version ((downloadable game-version) version)
  (let ((versions (get-versions downloadable)))
	(gethash version versions)))

(defgeneric download (downloadable path &key quiet))

(defmethod download ((downloadable downloadable) path &key quiet)
  (dl:download (url downloadable) path :quiet quiet))

(define-json-expander:define-json-expander mine-version-manfest ()
  ((latest
	:accessor latest
	:json-decoder #'decode-mine-latest)
   (versions
	:accessor versions
	:json-decoder (lambda (x)
					(let ((version-table (make-hash-table :test 'equal)))					  
					  (loop for obj in x
							for version = (decode-mine-versions obj)
							do (setf (gethash (name version) version-table) version))
					  version-table)))))

(define-json-expander:define-json-expander mine-versions (downloadable)
  ((display-name
	:json-prop :id
	:accessor name)
   (type
	:accessor type)
   (url
	:accessor url)
   (date
	:json-prop :time
	:accessor date)
   (release-date
	:json-prop :release-time
	:accessor release-date)))

(define-json-expander:define-json-expander mine-latest ()
  ((release
	:accessor release)
   (snapshot
	:accessor snapshot)))

(defun save-stream-to-file (stream output-path &key (buffer-size 4096))
  "Saves the data from STREAM to a file at OUTPUT-PATH using a buffer."
  (with-open-file (out-stream output-path
                    :direction :output
                    :if-exists :supersede
                    :if-does-not-exist :create
                    :element-type '(unsigned-byte 8)) ; Raw binary stream
    (let ((buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
      (loop
        with bytes-read
        do (setf bytes-read (read-sequence buffer stream))
        while (> bytes-read 0)
        do (write-sequence buffer out-stream :end bytes-read)))))




