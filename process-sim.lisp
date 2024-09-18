(in-package #:process-sim) `

(defparameter *processes*
  (list
   '(5 27 3 31 5 43 4 18 6 22 4 26 3 24 4)
   '(4 48 5 44 7 42 12 37 9 76 4 41 9 31 7 43 8)
   '(8 33 12 41 5 45 3 51 4 61 15 18 14 26 5 31 6)
   '(3 35 4 41 5 45 3 51 4 61 5 54 6 82 5 77 3)
   '(16 24 17 21 5 36 16 26 7 31 13 28 11 21 6 13 3 11 4)
   '(11 22 4 8 5 10 6 12 7 14 9 18 12 24 15 30 8)
   '(14 46 17 41 11 42 15 21 4 32 7 19 16 33 10)
   '(4 14 5 33 6 51 73 16 87 6)))

(defun sum-every-other (plist)
  (loop for n in plist
	for idx from 0
	when (= 0 (mod idx 2)) sum n))

(defun max-proc (pslist)
  (iter:iter (iter:for e in pslist)
    (iter:finding e iter:maximizing (sum-every-other e))))

(defun min-proc (pslist)
  (iter:iter (iter:for e in pslist)
    (iter:finding e iter:minimizing (sum-every-other e))))

(defun sum-procs (pslist)
  (loop for n in pslist
	collect (sum-every-other n)))

(defclass scheduler ()
  ((processes
    :initarg :processes
    :accessor processes)
   (waiting-times
    :initform '()
    :accessor waiting-times)
   (turnaround-times
    :initform '()
    :accessor turnaround-times)
   (response-times
    :initform '()
    :accessor response-times)
   (cpu-utilization
    :initform 0
    :accessor cpu-utilization)
   (strategy
    :initarg :strategy)))

(defun sum (xs)
  (loop for x in xs sum x))

;; Function that runs all processes in order
(defmethod run-proc-in-order ((sch scheduler))
  (let ((procs (processes sch))
	(waiting-time 0))
    (loop for (proc next-proc) on procs
	  do (setf waiting-time (+ (sum proc) waiting-time)))))

(defun shortest-proc-predicate (x y)
  (>
   (sum-every-other x)
   (sum-every-other y)))

(defgeneric shortest-proc (sch))

(defmethod shortest-proc ((sch scheduler))
  (sort (copy-seq (processes sch))
	#'shortest-proc-predicate))
