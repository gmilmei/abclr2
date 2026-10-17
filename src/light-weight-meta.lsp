;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Light-weight metaobject (LWM)
;;; Nov.10, 1991 by MASUHARA, H.
;;; Jul.25, 1992 --- Group manager become to be reifiable.
;;; Jun. 5, 1993 --- stupid bug that default meta-gen cannot be
;;;                  reified is fixed.

(in-package "ABCLR2-SYSTEM")

(defun LWM-den-obj (LWM) (first (object-data-local LWM)))
(defun LWM-reified (LWM) (not (eq (object-data-type LWM) 'LW-meta)))
(defun lock-obj (LWM) (cdr (object-data-local LWM)))
(defun lock-LWM-port (lock-obj)
  (lock lock-obj))
(defun unlock-LWM-port (lock-obj)
  (unlock lock-obj))  

(defun light-weight-meta-port (Me M R S)
  (if (and (consp M)
           (eq (first M) :message))
    ;; [:message Message Reply Sender]
    (apply #'send-message
           (LWM-den-obj Me)
           (rest M))
    ;; otherwise, reified object may understand the message
    (if (member (object-data-type (LWM-den-obj Me))
                '(Meta Eval GMgr Meta-gen) :test #'eq)
      ;; reifiable
      (send-message (LWM-den-obj Me)
        :reify-yourself
        (cont dummy
          (send-message Me M R S))
        Me)
      ;; not reifiable
      (progn
        (warn "The light-weight metaobject of ~S cannot handle the message ~S; because the object is not reifiable."
              (LWM-den-obj Me) M)))))

;; old definition
#+COMMENT
(defun light-weight-meta-port (Me M R S)
  (if (and (consp M)
	   (eq (first M) :message))
      ;; [:message Message Reply Sender]
      (apply #'send-message
	     (LWM-den-obj Me)
	     (rest M))
      ;; otherwise, reified object may understand the message
      (if (member (object-data-type (LWM-den-obj Me))
		  '(Meta Eval GMgr) :test #'eq)
	  ;; reifiable
	  (progn
	    (let ((lock-obj (lock-obj Me)))
	      ;; for anticipate simultaneous reification
	      (lock-LWM-port lock-obj)
	      (if (LWM-reified Me)
		  (progn
		    (unlock-LWM-port lock-obj)
		    (send-message Me M R S))
		  ;; Since the denoting evaluator can request reflective
		  ;; messages to its LWM, so this request must be sent
		  ;; by past type to avoid deadlock.
		  (progn
		    (send-message (LWM-den-obj Me)
		      :reify-yourself
		      (cont dummy
			(unlock-LWM-port lock-obj)
			(send-message Me M R S))
		      Me)))		; reify myself
	      ))
	  ;; not reifiable
	  (progn
	    (warn "The metaobject of ~S cannot handle the message ~S because~
                   the object cannot be reified."
		  (LWM-den-obj Me) M)
	    (break)))))			; send message again

(defun create-light-weight-meta-of (den-obj)
  (make-object-data 
   :port #'light-weight-meta-port
   :meta nil
   :den den-obj
   :gid (group-of (group-of den-obj))
   :local (cons den-obj (make-lock))
   :type 'LW-meta
   :status 'no-schedule
   :status-lock nil))

