
;;;; fabric.lisp

(in-package #:dev.shft.mineapi)

(define-json-expander:define-json-expander fabric-game-version (game-version)
  ((version
	:reader version)
   (stable
	:reader stablep)))

(define-json-expander:define-json-expander fabric-loader-version (loader-version)
  ((loader
	:reader loader
	:json-decoder #'decode-fabric-loader)
   (intermediary
	:reader intermediary)
   (launcher-meta
	:reader launcher-meta)))

(define-json-expander:define-json-expander fabric-loader ()
  ((separator
	:reader separator)
   (build
	:reader build)
   (maven
	:reader maven)
   (version
	:reader version)
   (stable
	:reader stable)))

;; (define-json-expander:define-json-expander fabric-loader-versions (version-manifest)
;;   ((versions
;; 	:reader versions
;; 	:json-decoder (lambda (x)
;; 					(loop for obj in x
;; 						  collect (decode-fabric-loader-version obj))))))


(defun decode-fabric-game-version (version)
  ())

(defclass fabric-loader-versions (version-manifest) ())

(defun decode-fabric-game-versions (versions)
  (let ((version-parsed (mapcar (lambda (x)
			(decode-fabric-game-version x)) versions)))
	(make-instance 'fabric-loader-versions :versions version-parsed)))


