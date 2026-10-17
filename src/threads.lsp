;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; threads scheduling for multi-threaded Common Lisp
;;; Nov. 7, 1991 by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

;; (init-threads &optional numebr-of-threads)
;; All threads previously created are killed.
;; Then new threads are created.

(defvar *abclr2-stack*
  ;; c    vs   frs  ihs
  '(7500 10000 1024 1024)
  "stack size for a thread")
(defvar *threads-list* nil)
(defvar *number-of-threads* nil)
(defvar *default-number-of-threads* 10)
(defvar *idle-threads* 0)
(defvar *idle-threads-lock*)
(defvar *threads-start* nil)

;;
;; counting idle threads
;;
(defun inc-idle-threads ()
  (si:process-lock *idle-threads-lock*)
  (incf *idle-threads*)
  (si:process-unlock *idle-threads-lock*))

(defun dec-idle-threads ()
  (si:process-lock *idle-threads-lock*)
  (decf *idle-threads*)
  (si:process-unlock *idle-threads-lock*))

(defun threads-exhausted-p ()
  (si:process-lock *idle-threads-lock*)
  (prog1 (zerop *idle-threads*)
   (si:process-unlock *idle-threads-lock*)))

;;
;; thread scheduling
;;
(defun idle-threads-loop ()
  (loop
   (catch 'thread-toplevel
     (do ()
	 (*threads-start*))
     ;; (format t "Thread ~S start to wait.~%" (si:process-self))
     (loop
      (let ((pid (fetch-process)))
	#+:ABCLR2-DEBUG(debug-print
			"thread ~A attatched to proecss ~A.~%"
			(si:process-self) pid)
	(dec-idle-threads)
	(execute-and-catch-error 'nrm-loop
	 (funcall (process-function pid) (process-object pid)))
	#+:ABCLR2-DEBUG(debug-print
			"process ~A is stopped.~%" pid)
	(inc-idle-threads)
	(schedule pid))))))

;;
;; initialization
;;
(defun init-threads (&optional (num-threads *default-number-of-threads*))
  ;; initialize variables
  (setf *idle-threads* num-threads
       *idle-threads-lock* (si:make-lock)
       *threads-list* nil)
  
  ;; creates new threads
  (format t "~&New threads are being created: ")
  (dotimes (i num-threads)
   (let* ((*stacksize* *abclr2-stack*)
         (tid (fork (idle-threads-loop))))
    (push tid *threads-list*)
    (format t "~A " tid)))
  (terpri)
  
  (setf *threads-start* t)
  )

(defun stop-threads ()
  (setf *threads-start* nil)
  (format t "Stopping threads:")
  (dolist (TID *threads-list*)
    (format t "~A " TID)
    (si:process-interrupt
     TID
     #'(lambda () (throw 'thread-toplevel nil))))
  (terpri)

  (setf *idle-threads* (length *threads-list*)
	*idle-threads-lock* (si:make-lock)))
  
(defun start-threads ()
  (setf *threads-start* t))
