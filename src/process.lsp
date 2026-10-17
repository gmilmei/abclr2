;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; process scheduling
;;; Nov. 9, 1991 by MASUHARA,H.
;;; Mar.22, 1993 variable *scheduler-running* is added.
;;; Jul.27, 1994 interrupt handler support

(in-package "ABCLR2-SYSTEM")

;; (create-process func obj)
;; A new process for obj driven by func is created.
;;
;; (init-processes)
;; Processes created so far are removed.
;;
;; (schedule pid)
;; Process pid is re-scheduled.
;;
;; (activate-process pid)
;; A dormant process pid will be pushed into scheduling queue.
;;
;; (fetch-process)
;; Pick up an active but not scheduled  process.
;;
;; (switch-context-p object)
;; Test the object should make context switching.
;;
;; (run-system)
;; for emulation by single-threaded Lisp, run active objects
;; until there are no active objects.
;;

(defvar *keep-process-list* nil)
(defvar *process-list* nil)
(defvar *process-list-lock* nil)
(defvar *process-scheduling-queue*)
(defvar *scheduler-running* nil)

;;
;; data structure of a process
;;
(defstruct (process
            (:include object)
            (:print-function print-process)
            (:constructor make-user-process))
  function object)

(defun print-process (x s l)
  (declare (ignore l))
  (format s "#<process~X:~A:~A>" (object-address x)
          (object-data-status (process-object x))
          (process-object x)))

;;
;; process list handling
;;
(defmacro with-locking (lock &rest form)
  #-KCLM (declare (ignore lock))
  `(progn #+KCLM (si:process-lock ,lock)
    (prog1 (progn ,@form)
      #+KCLM (si:process-unlock ,lock)
      )))

(defun add-process (pid)
  (with-locking *process-list-lock*
    (push pid *process-list*)))

(defun delete-process (pid)
  (with-locking *process-list-lock*
    (setf *process-list* (delete pid *process-list*))))

;;
;; A dormant object is activated when a message is sent.
;;
(defun activate-object (object)
  (when (eq 'dormant 
            (test-and-set-object-status object 'active))
    ;; when activated dormant object...
    (let ((pid (object-data-pid object)))
      #+:ABCLR2-DEBUG(debug-print
                            "~A is activated.~%" pid)
      (enqueue *process-scheduling-queue* pid))))

;;
;; Schedule a process to active or dormant pool. 
;;
(defun schedule (pid)
  (case (update-object-status (process-object pid))
    (dormant #+:ABCLR2-DEBUG(debug-print
                                   "~A goes to sleep.~%" pid)
             ;; do nothing
             )
    (active #+:ABCLR2-DEBUG(debug-print
                                  "~A goes to scheduling queue.~%" pid)
            (enqueue *process-scheduling-queue* pid))
    (dead #+:ABCLR2-DEBUG(debug-print
                                "~A is killed.~%" pid)
          (if *keep-process-list* (delete-process pid)))
    (no-schedule #+:ABCLR2-DEBUG
                 (debug-print "~A turns into non schedulable object~%"
                              pid)
                 (if *keep-process-list* (delete-process pid)))
    (t (warn "~A has illegal status." pid))))

;;
;; process creation
;;
(defun create-process (fun obj)
  (let ((pid (make-user-process :function fun :object obj)))
    #+:ABCLR2-DEBUG(debug-print
                          "~A createad.~%" pid)
    (setf (object-data-pid obj) pid)
    (if *keep-process-list* (add-process pid))
    (schedule pid)
    pid))

;;
;; fetch process
;;
(defun fetch-process ()
  (dequeue *process-scheduling-queue*))

;; The current status of a object is COMPUTED and returned.
;; The returned status may not match the status slot of the object.
;; If the object is not 'active' nor 'dormant,' nil is returned.
;; This operation is not atomic, so the actual status can changed
;; at any time. 
(defun prove-object-status (object)
  (if (member (object-data-status object) 
               '(active dormant)
               :test #'eq)
       (and (queue-p (object-data-port object)) ; safety
            (if (emptyp (object-data-port object))
              'dormant 'active))
       (object-data-status object)))

;; The status of the object is modified, and old status is returned.
;; This is atomic operation.
(defun test-and-set-object-status (object new-status)
  (lock (object-data-status-lock object))
  (prog1 (object-data-status object)
    (setf (object-data-status object) new-status)
  (unlock (object-data-status-lock object)))
  )

;; Atomic update of object's status
(defun update-object-status (object)
  (lock (object-data-status-lock object))
  (prog1 (setf (object-data-status object)
               (prove-object-status object))
    (unlock (object-data-status-lock object))
    ))

;;
;; test for context switching
;;
(defun switch-context-p (object)
  (let ((status (prove-object-status object)))
    #+KCLM
    (or
     ;; 1. Stopped processes cannot run any more.
     (eq 'dead status)
     ;; 2. When there are more active processes than threads,
     ;;    the system is scheduled by the round-robin style.
     (threads-exhausted-p)
     ;; 3. Processes which have more messages to handle
     ;;    continues its execution.
     (not (eq status 'active)))
    #-KCLM
    (or 
     ;; 1. If there are any process waiting for schedule,
     ;;    context should be switched.
     (not (emptyp *process-scheduling-queue*))
     ;; 2. A process which has no messages should swtich.
     (not (eq 'active status)))
    ))

;;
;; loop for single thread
;;
(defun run-system (&optional extra-termination)
  (let ((*scheduler-running* t))
    (do ()
        ((or (and extra-termination
                  (funcall extra-termination))
             (emptyp *process-scheduling-queue*)) nil)
      ;;(format t "~&***queue=~S***" *process-scheduling-queue*)
      (check-signal-request)		;Wed Jul 27 19:35:10 JST 1994
      (let ((pid (fetch-process)))
        (execute-and-catch-error
         'nrm-loop
         (funcall (process-function pid) 
                  (process-object pid)))
        (schedule pid)))))

;;
;; killing process for a object
;;
(defun kill-process-for (object)
  (setf (object-data-status object) 'dead))

;;
;; initialization
;;
(defun init-processes (&optional (threads nil))
  (when *keep-process-list*
    (format t "cleaning old processes~% ~A~%"
	    *process-list*)
    (setf *process-list* nil)
    #+KCLM (setf *process-list-lock* (si:make-lock)))
  (setf *process-scheduling-queue* (make-queue))
  #+KCLM (if threads (init-threads threads) (init-threads))
  )

(defun reset-processes (&optional (threads nil))
  #+KCLM (stop-threads)
  (when *keep-process-list*
    (format t "cleaning old processes~% ~A~%"
	    *process-list*)
    (setf *process-list* nil) 
    #+KCLM (setf *process-list-lock* (si:make-lock)))
  (setf *process-scheduling-queue* (make-queue))
  #+KCLM (start-threads)
  )
