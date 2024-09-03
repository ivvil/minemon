;;;; system.lisp

(in-package #:dev.shft.minemon)

(defparameter *reserved-system-memory* 0.5
  "Percentage of memory that will be left for the system when calculating recommended memory sizes")

(defun recommended-memory-size (&optional instances)		;MAYBE Take into account base memory use
  "Returns the recommended memory for a server based on system memory in Gibibytes.
INSTANCES is the ammount of instances to account for"
  (let* ((total-memory (nth-value 1 (machine-room)))
		 (recommemded-memory (* total-memory *reserved-system-memory*))
		 (instance-memory (/ recommemded-memory (or instances 1))))
	(nth-value 0 (round (bytes-tog-gib instance-memory)))))

(defun bytes-tog-gib (bytes)
  (/ bytes (expt 1024 3)))

(defun get-uptime () ; FIXME Posix only
  (let* ((uptime (read-file-string "/proc/uptime"))
		 (space (position #\Space uptime)))
	(cons (parse-float (subseq uptime space))
		  (parse-float (subseq uptime 0 space)))))


