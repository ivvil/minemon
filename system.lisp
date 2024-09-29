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
  (member
   running
   sleeping
   waiting
   zombie
   stopped
   tracing
   dead
   idle))

(defun get-proc-stat (pid)
  ())

(defclass proc-pid-stat ()
  ((pid
	:initarg :pid
	:reader pid
	:documentation "pid of the process")
   (comm
	:initarg :comm
	:reader comm
	:documentation "The  filename  of  the  executable,  in  parentheses.")
   (state
	:initarg :nice
	:reader nice
	:type state
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
	:documentation "The number of minor faults the process has made which have not required loading a memory page from disk")
   (cminflt
	:initarg :cminflt
	:reader cminflt
	:documentation "The number of minor faults that the process's waited-for children have made")
   (majflt
	:initarg :majflt
	:reader majflt
	:documentation "The  number  of major faults the process has made which have required loading a memory page from disk")
   (cmajflt
	:initarg :cmajflt
	:reader cmajflt
	:documentation "The number of major faults that the process's waited-for children have made")
   (utime
	:initarg :utime
	:reader utime
	:documentation "Amount of time that this process has been scheduled in user mode,  measured  in clock  ticks")
   (stime
	:initarg :stime
	:reader stime
	:documentation "Amount of time that this process has been scheduled in kernel mode, measured in clock ticks")
   (cutime
	:initarg :cutime
	:reader cutime
	:documentation "Amount of time that this process's waited-for children have been  scheduled  in user mode, measured in clock ticks")
   (cstime
	:initarg :cstime
	:reader cstime
	:documentation "Amount  of  time that this process's waited-for children have been scheduled in kernel mode, measured in clock ticks")
   (priority
	:initarg :priority
	:reader priority
	:documentation "The negated scheduling priority, minus one")
   (nice
	:initarg :nice
	:reader nice
	:documentation "The nice value")
   (num-threads
	:initarg threads
	:reader num-threads
	:documentation "Number  of  threads in this process")
   (itrealvalue
	:initarg :itrealvalue
	:reader itrealvalue
	:documentation "The time in jiffies before the next SIGALRM is sent to the process due to an interval timer.")
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
	:documentation "Resident Set Size: number of pages the process has in real  memory")
   (rsslim
	:initarg :rsslim
	:reader rsslim
	:documentation "Current  soft limit in bytes on the rss of the process")
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
	:documentation "The address of the start (i.e., bottom) of the stack")
   (kstkesp
	:initarg :kstkesp
	:reader kstkesp
	:documentation "The current value of ESP (stack pointer), as found in the kernel stack page for the process")
   (kstkeip
	:initarg :kstkeip
	:reader kstkeip
	:documentation "The current EIP (instruction pointer)")
   (signal
	:initarg :signal
	:reader proc-signal
	:documentation "The  bitmap  of pending signals, displayed as a decimal number")
   (blocked
	:initarg :blocked
	:reader blocked
	:documentation "The bitmap of blocked signals, displayed as a decimal number")
   (sigignore
	:initarg :sigignore
	:reader sigignore
	:documentation "The  bitmap  of ignored signals, displayed as a decimal number")
   (sigcatch
	:initarg :sigcatch
	:reader sigcatch
	:documentation "The bitmap of caught signals, displayed as a decimal  number")
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
	:documentation "Signal to be sent to parent when we die")
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
	:documentation "Aggregated block I/O delays, measured in clock ticks (centiseconds)")
   (guest-time
	:initarg :guest-time
	:reader guest-time
	:documentation "Guest  time of the process (time spent running a virtual CPU for a guest operating system), measured in clock ticks")
   (cguest-time
	:initarg :cguest-time
	:reader cguest-time
	:documentation "Guest time of the process's children, measured in clock ticks (divide  by sysconf(_SC_CLK_TCK)")
   (start-data
	:initarg :start-data
	:reader start-data
	:documentation "Address  above  which  program  initialized  and  uninitialized (BSS) data are placed")
   (end-data
	:initarg :end-data
	:reader end-data
	:documentation "Address below which program initialized and uninitialized (BSS) data are placed")
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
