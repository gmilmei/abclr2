;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; semaphore functions --- for emulating by non-multi threaded Lisp

;; As "waiting for signal" is meaningless, behaviors of semaphore
;; functions differ between multi-threaded and non-multi threaded
;; Lisp.

(in-package "ABCLR2-SYSTEM")

;;(in-package 'lisp)
  
(export '(make-semaphore
	  semaphore-p
	  semaphore-wait
	  semaphore-signal))

;; Be careful to use! These semaphore functions do not wait at all.

(defstruct (semaphore
            (:include object)
            (:print-function semaphore-print))
  (counter 0 :type fixnum)
  que)

(defun semaphore-print (x s l)
  (declare (ignore l))
  (format s "#<semaphore:~A>" (object-address x)))

(defun semaphore-wait (sem)
  (if (zerop (semaphore-counter sem))
      nil
      (progn (decf (semaphore-counter sem))
             t)))

(defun semaphore-test (sem)
  (not (zerop (semaphore-counter sem))))

(defun semaphore-signal (sem)
  (incf (semaphore-counter sem)))
