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

(deftype proc-state ()
  '(member
	running
	sleeping
	waiting
	zombie
	stopped
	tracing
	dead
	idle))

(defun parse-proc-state (str)
  (case str
    (#\R 'running)
    (#\S 'sleeping)
    (#\D 'waiting)
    (#\Z 'zombie)
    (#\T 'stopped)  ; Can include tracing in this as well
    (#\X 'dead)
    (#\I 'idle)
    (t (error "Unknown process state: ~A" str))))

(defun parse-stat-string (stat)
  (if stat
  (let* ((start-comm (position #\( stat))  ; Find the opening parenthesis of the comm
         (end-comm (position #\) stat))    ; Find the closing parenthesis of the comm
         ;; Extract the process name (comm) inside parentheses
         (comm (subseq stat (1+ start-comm) end-comm))
         ;; Split the remaining stat string after the closing parenthesis
         (prefix (subseq stat 0 start-comm))
         (suffix (subseq stat (1+ end-comm)))
         (fields (split-sequence #\Space (concatenate 'string prefix suffix))))
    ;; Convert the fields to a vector for fast access
    (let ((vec (coerce fields 'vector)))
      ;; Create an instance of proc-pid-stat
      (make-instance 'proc-pid-stat
                     :pid (elt vec 0)
                     :comm comm
                     :state (parse-proc-state (elt (elt vec 2) 0))  ; 'state' is a char
                     :ppid (elt vec 3)
                     :pgrp (elt vec 4)
                     :session (elt vec 5)
                     :tty-nr (elt vec 6)
                     :tpgid (elt vec 7)
                     :flags (elt vec 8)
                     :minflt (elt vec 9)
                     :cminflt (elt vec 10)
                     :majflt (elt vec 11)
                     :cmajflt (elt vec 12)
                     :utime (elt vec 13)
                     :stime (elt vec 14)
                     :cutime (elt vec 15)
                     :cstime (elt vec 16)
                     :priority (elt vec 17)
                     :nice (elt vec 18)
                     :threads (elt vec 19)
                     :itrealvalue (elt vec 20)
                     :starttime (elt vec 21)
                     :vsize (elt vec 22)
                     :rss (elt vec 23)
                     :rsslim (elt vec 24)
                     :startcode (elt vec 25)
                     :endcode (elt vec 26)
                     :startstack (elt vec 27)
                     :kstkesp (elt vec 28)
                     :kstkeip (elt vec 29)
                     :signal (elt vec 30)
                     :blocked (elt vec 31)
                     :sigignore (elt vec 32)
                     :sigcatch (elt vec 33)
                     :wchan (elt vec 34)
                     :nswap (elt vec 35)
                     :cnswap (elt vec 36)
                     :exit-signal (elt vec 37)
                     :processor (elt vec 38)
                     :rt-priority (elt vec 39)
                     :policy (elt vec 40)
                     :delayacct-blkio-ticks (elt vec 41)
                     :guest-time (elt vec 42)
                     :cguest-time (elt vec 43)
                     :start-data (elt vec 44)
                     :end-data (elt vec 45)
                     :start-brk (elt vec 46)
                     :arg-start (elt vec 47)
                     :arg-end (elt vec 48)
                     :env-start (elt vec 49)
                     :env-end (elt vec 50)
                     :exit-code (elt vec 51))))
   (error "Invalid stat data, unable to parse.")))


(defun get-stat-string (pid)
  (let ((stat-path (merge-pathnames (concatenate 'string (write-to-string pid) "/") "/proc/stat")))
	(uiop:read-file-string stat-path :external-format :iso-8859-1)))

(defclass proc-pid-stat ()
  ((pid
    :initarg :pid
    :reader pid
    :documentation "pid of the process")
   (comm
    :initarg :comm
    :reader comm
    :documentation "The filename of the executable, in parentheses.")
   (state
    :initarg :state
    :reader state
    :type proc-state
    :documentation "State of the process")
   (ppid
    :initarg :ppid
    :reader ppid
    :documentation "The PID of the parent of this process")
   (pgrp
    :initarg :pgrp
    :reader pgrp
    :documentation "The process group ID of the process")
   (session
    :initarg :session
    :reader session
    :documentation "The session ID of the process")
   (tty-nr
    :initarg :tty-nr
    :reader tty-nr
    :documentation "The controlling terminal of the process")
   (tpgid
    :initarg :tpgid
    :reader tpgid
    :documentation "The ID of the foreground process group of the controlling terminal of the process")
   (flags
    :initarg :flags
    :reader flags
    :documentation "The kernel flags word of the process")
   (minflt
    :initarg :minflt
    :reader minflt
    :documentation "The number of minor faults the process has made")
   (cminflt
    :initarg :cminflt
    :reader cminflt
    :documentation "The number of minor faults that the process's waited-for children have made")
   (majflt
    :initarg :majflt
    :reader majflt
    :documentation "The number of major faults the process has made")
   (cmajflt
    :initarg :cmajflt
    :reader cmajflt
    :documentation "The number of major faults that the process's waited-for children have made")
   (utime
    :initarg :utime
    :reader utime
    :documentation "Amount of time that this process has been scheduled in user mode")
   (stime
    :initarg :stime
    :reader stime
    :documentation "Amount of time that this process has been scheduled in kernel mode")
   (cutime
    :initarg :cutime
    :reader cutime
    :documentation "Amount of time that this process's waited-for children have been scheduled in user mode")
   (cstime
    :initarg :cstime
    :reader cstime
    :documentation "Amount of time that this process's waited-for children have been scheduled in kernel mode")
   (priority
    :initarg :priority
    :reader priority
    :documentation "The negated scheduling priority, minus one")
   (nice
    :initarg :nice
    :reader nice
    :documentation "The nice value")
   (num-threads
    :initarg :threads
    :reader num-threads
    :documentation "Number of threads in this process")
   (itrealvalue
    :initarg :itrealvalue
    :reader itrealvalue
    :documentation "The time before the next SIGALRM is sent to the process")
   (starttime
    :initarg :starttime
    :reader starttime
    :documentation "The time the process started after system boot")
   (vsize
    :initarg :vsize
    :reader vsize
    :documentation "Virtual memory size in bytes")
   (rss
    :initarg :rss
    :reader rss
    :documentation "Resident Set Size: number of pages the process has in real memory")
   (rsslim
    :initarg :rsslim
    :reader rsslim
    :documentation "Current soft limit in bytes on the rss of the process")
   (startcode
    :initarg :startcode
    :reader startcode
    :documentation "The address above which program text can run")
   (endcode
    :initarg :endcode
    :reader endcode
    :documentation "The address below which program text can run")
   (startstack
    :initarg :startstack
    :reader startstack
    :documentation "The address of the start (bottom) of the stack")
   (kstkesp
    :initarg :kstkesp
    :reader kstkesp
    :documentation "The current value of ESP (stack pointer)")
   (kstkeip
    :initarg :kstkeip
    :reader kstkeip
    :documentation "The current EIP (instruction pointer)")
   (signal
    :initarg :signal
    :reader proc-signal
    :documentation "The bitmap of pending signals")
   (blocked
    :initarg :blocked
    :reader blocked
    :documentation "The bitmap of blocked signals")
   (sigignore
    :initarg :sigignore
    :reader sigignore
    :documentation "The bitmap of ignored signals")
   (sigcatch
    :initarg :sigcatch
    :reader sigcatch
    :documentation "The bitmap of caught signals")
   (wchan
    :initarg :wchan
    :reader wchan
    :documentation "This is the \"channel\" in which the process is waiting")
   (nswap
    :initarg :nswap
    :reader nswap
    :documentation "Number of pages swapped")
   (cnswap
    :initarg :cnswap
    :reader cnswap
    :documentation "Cumulative nswap for child processes")
   (exit-signal
    :initarg :exit-signal
    :reader exit-signal
    :documentation "Signal to be sent to parent when process dies")
   (processor
    :initarg :processor
    :reader processor
    :documentation "CPU number last executed on")
   (rt-priority
    :initarg :rt-priority
    :reader rt-priority
    :documentation "Real-time scheduling priority")
   (policy
	   :initarg :policy
	   :reader policy
	   :documentation "Scheduling policy")
   (delayacct-blkio-ticks
    :initarg :delayacct-blkio-ticks
    :reader delayacct-blkio-ticks
    :documentation "Aggregated block I/O delays")
   (guest-time
    :initarg :guest-time
    :reader guest-time
    :documentation "Guest time of the process")
   (cguest-time
    :initarg :cguest-time
    :reader cguest-time
    :documentation "Guest time of the process's children")
   (start-data
    :initarg :start-data
    :reader start-data
    :documentation "Address above program initialized and uninitialized (BSS) data")
   (end-data
    :initarg :end-data
    :reader end-data
    :documentation "Address below program initialized and uninitialized (BSS) data")
   (start-brk
    :initarg :start-brk
    :reader start-brk
    :documentation "Address above which program heap can be expanded with brk")
   (arg-start
    :initarg :arg-start
    :reader arg-start
    :documentation "Address above which program command-line arguments (argv) are placed")
   (arg-end
    :initarg :arg-end
    :reader arg-end
    :documentation "Address below program command-line arguments (argv) are placed")
   (env-start
    :initarg :env-start
    :reader env-start
    :documentation "Address above which program environment is placed")
   (env-end
    :initarg :env-end
    :reader env-end
    :documentation "Address below which program environment is placed")
   (exit-code
    :initarg :exit-code
    :reader exit-code
    :documentation "The thread's exit status in the form reported by waitpid")))
