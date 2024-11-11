;;;; utils.lisp

(in-package #:dev.shft.minemon)

(defun create-temporary-dir ()
  (ensure-directories-exist (merge-pathnames (temporary-directory) )))

(defun print-time (&optional (seconds (get-universal-time)))
  "Convert SECONDS into a human-readable time format."
  (let* ((days (floor seconds 86400))               ; 86400 seconds in a day
         (remaining-seconds (mod seconds 86400))
         (hours (floor remaining-seconds 3600))     ; 3600 seconds in an hour
         (remaining-seconds (mod remaining-seconds 3600))
         (minutes (floor remaining-seconds 60))     ; 60 seconds in a minute
         (seconds (mod remaining-seconds 60)))
    (format nil "~@[~a days, ~]~@[~a hours, ~]~@[~a minutes, ~]~a seconds"
            (and (> days 0) days)
            (and (> hours 0) hours)
            (and (> minutes 0) minutes)
            seconds)))

