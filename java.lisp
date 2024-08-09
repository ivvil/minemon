;;;; java.lisp

(in-package #:dev.shft.minemon)

(defparameter *javatest-path* #P"JavaTest.java ")

(defclass java-install ()
  ((name
	:initarg :name
	:initform "java"
	:accessor name)
   (arch
	:initform "unknown"
	:accessor arch)
   (path
	:initarg :path
	:accessor path)))

(defgeneric java-version (java))

(defmethod java-version ((java string))
  (if (string-prefix-p "1.")))

(defmethod java-version ((java java-install))
  )

(defun java-paths-linux ()
  )

(defun default-java ()
  )

(defun parse-java-system-properties-to-hashtable (sysprop-xmls)
  (let ((xpath:*navigator* (cxml-xmls:make-xpath-navigator))
		(ht (make-hash-table :test 'equal)))
	(loop for entry in (xpath:evaluate "//entry" sysprop-xmls))))

(defun parse-java-system-properties-to-hashtable (sysprop-stream)
  (let ((ht (make-hash-table :test 'equal))
		(doc (cxml:make-source sysprop-stream :entity-resolver (lambda (pubid sysid)
																 (declare (ignore pubid))
																 (when (eq (puri:uri-scheme sysid) :http)
																   (drakma:http-request sysid :want-stream t))))))
	(loop
	  for key = (klacks:peek doc)
	  while key
	  do
		 (when (eql key :start-element)
		   (klacks:map-attributes (lambda (namespace-uri local-name qualified-name attribute-value specified-p)
									(declare (ignore namespace-uri local-name qualified-name specified-p))
									(setf (gethash attribute-value ht) (remove-if (lambda (char) (member char '(#\Newline #\Tab))) (string-trim " " (nth-value 1 (klacks:peek-next doc)))))
									) doc))
		 (klacks:consume doc))
	ht))
