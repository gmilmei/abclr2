;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Time Special form
;;; Mar. 15, 1992 by MASUHARA, H.
;;; Apr. 192 1992 --- bug fix

(in-package "ABCLR2-SYSTEM")

(defun timer-loop (Me)
  (primary-object-loop 
   Me ((port (object-data-port Me)))
   port (M C S)
   (declare (ignore C S))
   ;; M should be a <C Exp Env Id Gid Eval>
   (let ((val nil))
   (time
    (setq val (send-present-message (sixth M)
                (cons ':do (rest M)) Me)))
   (send-message (first M) val nil Me))))

(defun create-timer (&optional Group)
  (let ((timer (make-object-data 
                :port (create-queue)
                :meta nil
                :den nil
                :gid Group
                :type 'NRM
                :status 'dormant
                :name 'TIMER)))
    (create-process #'timer-loop timer)
    timer))
