;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; semaphore functions for TOP-1 Common Lisp

(in-package "ABCLR2-SYSTEM")

;;   In multi-threaded Lisp, semaphore-wait returns immidiately
;; when the semaphore had been signalled, and wait the execution 
;; of the process when the semaphore had not been signalled.
;;   On the other hand, in non-multi threaded Lisp, semaphore-wait
;; returns t when the semaphore had been signalled, and returns nil
;; when not. Thus, codes for context switching is required when
;; the semaphore-wait returns nil.

(in-package 'lisp)
  
(export '(make-semaphore
	  semaphore-p
	  semaphore-wait
	  semaphore-signal))
  
(defstruct (semaphore
            (:include object)
            (:print-function
             (lambda (x s l)
               (declare (ignore l))
               (format s "#<semaphore ~X>" (si:address x))))
            (:constructor make-semaphore (&optional count))
            (:copier nil)
            )
  (lock (si:make-lock))
  (count 0 :type fixnum)
  que)

(defun semaphore-wait (sem)
  (let ((lock (semaphore-lock sem)))
    (si:process-lock lock)
    (cond ((< (the fixnum (decf (the fixnum (semaphore-count sem)))) 0)
	   (setf (semaphore-que sem)
		 (nconc (semaphore-que sem) (cons (si:process-self) nil)))
	   (si:process-unlock lock)
	   (cond ((si:process-wait)
		  (si:process-lock lock)
		  (setf (semaphore-que sem)
			(delete (si:process-self) (semaphore-que sem)))
		  (si:process-unlock lock)
		  t)
		 (t nil)))
	  (t
	   (si:process-unlock lock)
	   nil))))

(defun semaphore-signal (sem)
  (let ((lock (semaphore-lock sem)))
    (si:process-lock lock)
    (cond ((<= (the fixnum (incf (the fixnum (semaphore-count sem)))) 0)
	   (si:process-resume (pop (semaphore-que sem)))
	   (si:process-unlock lock)
	   t)
	  (t
	   (si:process-unlock lock)
	   nil))))
