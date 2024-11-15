;;;; package.lisp

(defpackage #:dev.shft.mineapi
  (:use #:cl
		#:define-json-expander)
  (:local-nicknames (:json #:cl-json)
					(:dl #:trivial-download)))
