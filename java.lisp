;;;; java.lisp

(in-package #:dev.shft.minemon)

(defparameter *javatest-path* (asdf:system-relative-pathname "minemon" "bin/java/JavaTest.jar"))

(defclass java-install ()
  ((name
	:initarg :name
	:initform "java"
	:accessor name)
   (arch
	:initarg :arch
	:initform "unknown"
	:accessor arch)
   (path
	:initarg :path
	:accessor path)))

(defgeneric java-version (java))

(defmethod java-version ((java string))
  ;; (if (string-prefix-p "1."))
  )

(defmethod java-version ((java java-install))
  )

(defun compile-to-jar (javafile)
  (run-program `("javac"
					"-source" "8"
					"-target" "8"
					"-Xlint:-options"
					"-d" ,(princ-to-string uiop:*temporary-directory*)
					,(princ-to-string *javatest-path*)))
  (run-program `("jar"
				 "cf"
				 "-d" ,(princ-to-string uiop:*temporary-directory*)
				 ,(princ-to-string *javatest-path*)))
  (merge-pathnames uiop:*temporary-directory* (make-pathname :name "JavaTest" :type "class")))

(defun java-paths-linux ())

(defun run-jar (java jar)
  (launch-program `(,(path java)
					"-jar"
					,(princ-to-string jar))
				  :output :stream))

(defun run-javatest (java)
  (run-jar java *javatest-path*))

(defun get-java-sysprop (java-instance)
  "Gets Java system properties for the given JAVA-INSTANCE."
  (let* ((process (run-javatest java-instance))
         (stream (process-info-output process)))
    (if stream
        (let ((hash (parse-java-system-properties-to-hashtable stream)))
          (log:debug "Sysprop:" hash)
          hash)  ; Return the hash for further use if needed
        (log:error "Failed to retrieve system properties for Java instance:" java-instance))))

(defun default-java ()
  (make-instance 'java-install :name "java" :arch "unknown" :path "java"))

;; Not downloading the DTD speeds up this up a lot, we could cache it so it only waits for it on the first try
(defun parse-java-system-properties-to-hashtable (sysprop-stream) ;TODO Add a way of passing a list of keys to optimize search (normally we only want 2 or 3 keys)
  ;; (declare (optimize (speed 3) (safety 0)))
  (let ((ht (make-hash-table :test 'equal))
		(doc (cxml:make-source sysprop-stream :entity-resolver (lambda (pubid sysid)
																 (declare (ignore pubid sysid))
																 ;; (when (eq (puri:uri-scheme sysid) :http)
																 ;;   (drakma:http-request sysid :want-stream t))
																 (flexi-streams:make-in-memory-input-stream nil)))))
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
