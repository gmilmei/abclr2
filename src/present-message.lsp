;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; present message handler
;;; Nov. 4, 1991 by MASUHARA,H.

(in-package "ABCLR2-SYSTEM")

(defvar *present-message-waiters* nil)

(defun reset-present-message ()
  (setf *present-message-waiters* nil))

;; In non-multi-threaded Lisp, only one process can wait for a
;; present type message. And this feature is provided for
;; top-level debugging.

(defun message-receiver-port (receiver Message Reply Sender)
  (declare (ignore Reply Sender))
  (setf (second (object-data-local receiver))
	Message)
  (semaphore-signal (car (object-data-local receiver))))

(defun create-message-receiver (message-body sender target)
  (make-object-data :port #'message-receiver-port
                    :local (list (make-semaphore) nil 
                                 message-body sender target)
                    :meta nil
                    :den nil
                    :type 'Cont
                    :status 'no-schedule
                    :status-lock nil))

#+KCLM
(defun wait-message (message-receiver)
  (push message-receiver *present-message-waiters*)
  (semaphore-wait (first (object-data-local message-receiver)))
  (setf *present-message-waiters*
        (delete message-receiver *present-message-waiters*))
  (second (object-data-local message-receiver)))

#-KCLM
(defun wait-message (message-receiver)
  (let ((sem (first (object-data-local message-receiver))))
    #+:ABCLR2-DEBUG
    (if (> *number-of-present-message-waiters* 0)
	(warn "Multiple waiting for present messages.~%~
             This may cause dead-lock."))
    (push message-receiver *present-message-waiters*)  
    (do ()
        ((semaphore-test sem))
      (if (emptyp *process-scheduling-queue*)
        (blocked-message-debugger message-receiver)
        (run-system #'(lambda () (semaphore-test sem)))))
    
    (setf *present-message-waiters*
          (delete message-receiver *present-message-waiters*))   
    (second (object-data-local message-receiver))
    ))

(defun send-present-message (target message sender)
  (let ((receiver (create-message-receiver message sender target)))
    (send-message target message receiver sender)
    (wait-message receiver)))

(defun show-blocked-messages (&aux (count 0))
  (let ((*print-length* 3))
    (format t "~&Blocked messages:")
    (dolist (MST (mapcar #'(lambda (receiver)
                            (cddr (object-data-local receiver)))
                        *present-message-waiters*))
     (format t "~&~D:Target=~S Message=~S Sender=~S"
            count (third MST) (first MST) (second MST))
     (incf count))))

(defun blocked-message-debugger (receiver &aux number value)
  (ABCLR2-WARN "No return messages!")
  (show-blocked-messages)
  (format t "~&Return value=")
  (setq value (read))
  (send-message receiver value nil nil))
                     
