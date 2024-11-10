;;;; system.lisp

(in-package #:dev.shft.minemon.system)

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
                         :pid (parse-integer (elt vec 0))
                         :comm comm
                         :state (parse-proc-state (elt (elt vec 2) 0))  ; 'state' is a char
                         :ppid (parse-integer (elt vec 3))
                         :pgrp (parse-integer (elt vec 4))
                         :session (parse-integer (elt vec 5))
                         :tty-nr (parse-integer (elt vec 6))
                         :tpgid (parse-integer (elt vec 7))
                         :flags (parse-integer (elt vec 8))
                         :minflt (parse-integer (elt vec 9))
                         :cminflt (parse-integer (elt vec 10))
                         :majflt (parse-integer (elt vec 11))
                         :cmajflt (parse-integer (elt vec 12))
                         :utime (parse-integer (elt vec 13))
                         :stime (parse-integer (elt vec 14))
                         :cutime (parse-integer (elt vec 15))
                         :cstime (parse-integer (elt vec 16))
                         :priority (parse-integer (elt vec 17))
                         :nice (parse-integer (elt vec 18))
                         :threads (parse-integer (elt vec 19))
                         :itrealvalue (parse-integer (elt vec 20))
                         :starttime (parse-integer (elt vec 21))
                         :vsize (parse-integer (elt vec 22))
                         :rss (parse-integer (elt vec 23))
                         :rsslim (parse-integer (elt vec 24))
                         :startcode (parse-integer (elt vec 25))
                         :endcode (parse-integer (elt vec 26))
                         :startstack (parse-integer (elt vec 27))
                         :kstkesp (parse-integer (elt vec 28))
                         :kstkeip (parse-integer (elt vec 29))
                         :signal (parse-integer (elt vec 30))
                         :blocked (parse-integer (elt vec 31))
                         :sigignore (parse-integer (elt vec 32))
                         :sigcatch (parse-integer (elt vec 33))
                         :wchan (parse-integer (elt vec 34))
                         :nswap (parse-integer (elt vec 35))
                         :cnswap (parse-integer (elt vec 36))
                         :exit-signal (parse-integer (elt vec 37))
                         :processor (parse-integer (elt vec 38))
                         :rt-priority (parse-integer (elt vec 39))
                         :policy (parse-integer (elt vec 40))
                         :delayacct-blkio-ticks (parse-integer (elt vec 41))
                         :guest-time (parse-integer (elt vec 42))
                         :cguest-time (parse-integer (elt vec 43))
                         :start-data (parse-integer (elt vec 44))
                         :end-data (parse-integer (elt vec 45))
                         :start-brk (parse-integer (elt vec 46))
                         :arg-start (parse-integer (elt vec 47))
                         :arg-end (parse-integer (elt vec 48))
                         :env-start (parse-integer (elt vec 49))
                         :env-end (parse-integer (elt vec 50))
                         :exit-code (parse-integer (elt vec 51)))))
    (error "Invalid stat data, unable to parse.")))

(defun get-stat-string (pid)
  (let ((stat-path (merge-pathnames (concatenate 'string (write-to-string pid) "/") "/proc/stat")))
	(uiop:read-file-string stat-path :external-format :iso-8859-1)))

(defun get-pid-stat (pid)
  (parse-stat-string (get-stat-string pid)))

(defun cpu-usage (pid-stat &optional clock-speed uptime)
  (let* ((clock-speed (or clock-speed (get-clk-tck)))
		 (uptime (or uptime (car (get-uptime))))
		 (total-time (/ (+ (utime pid-stat)
                           (stime pid-stat)
                           ;; (cutime pid-stat)
                           ;; (cstime pid-stat)
						   )
                        clock-speed))  ; Convert from clock ticks to seconds
         (seconds (- uptime
                     (/ (starttime pid-stat)
						clock-speed)))  ; Convert start time to seconds
         (cpu-usage (/ (* total-time
						  100)
					   seconds)))  ; Percentage CPU usage
    cpu-usage))

(defun get-cpu-usage (pid)
  (cpu-usage (get-pid-stat pid)))

(cffi:defcenum sysconf-constants
  (_SC_ARG_MAX 0)
  (_SC_CHILD_MAX 1)
  (_SC_CLK_TCK 2)  ;; On most systems, this is 2 but now determined by the C library
  (_SC_OPEN_MAX 3)
  (_SC_NGROUPS_MAX 4))

(cffi:defcfun "sysconf" :long
  (name sysconf-constants))

(defun get-clk-tck ()
  "Retrieve the number of clock ticks per second using sysconf."
  (sysconf '_SC_CLK_TCK))

(cffi:defcfun ("sysconf" get-page-size) :long
  (name :int))

(defun get-pagesize ()
  "Retrieve the system's page size in bytes."
  (get-page-size 30)) ;; 30 corresponds to _SC_PAGESIZE

(defun get-ram-usage (pid)
  "Get the RAM usage of the process with the given PID."
  (let* ((statm-path (concatenate 'string "/proc/" (write-to-string pid) "/statm"))
         (statm-string (with-open-file (stream statm-path :direction :input)
						 (read-line stream)))
		 (statm-data (mapcar #'parse-integer (split-sequence #\Space statm-string))))
	(* (second statm-data)
	   (get-pagesize))))

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
