;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; toplevel object
;;; Dec. 22, 1991 by MASUHARA, H.
;;; May 8, 1992 by MASUHARA, H. for CMU-CLv.16b
;;; June 7, 1992 by MASUHARA, H. for MCL 2.0b1
;;;	With MCL 2.0b1, toplevel object reads user inputs with
;;;	*interactive-readtable*.
;;; Jul.19, 1992 --- Toplevel evaluator is bound to *eval* in global-env.
;;; Jul.21, 1992 --- a bug of error detection is fixed.
;;; Jul.24, 1992 --- initial global-env is created by pairlis (for CLtL2)
;;; Jul.29, 1992 --- global variables for require&provide are introduced.
;;; Aug. 8, 1992 --- with MCL 2.0f, *interactive-readtable* is no
;;; 		     longer needed
;;; Jan.11, 1993 --- Restart facility is added.
;;; Mar.23, 1993 --- listener window is created in MCL
;;; Jul.27, 1994 --- interrupt handler support.

(in-package "ABCLR2-SYSTEM")

#+:CCL
(defclass abclr2-listener (ccl:listener)
  ())
#+:CCL
(defmethod ccl::initialize-window :after ((w abclr2-listener) &key)
  (ccl:set-window-title w "ABCL/R2"))  
#+:CCL
(defvar *abclr2-listener* nil
  "Listener window for ABCL/R2 system.")
#+:CCL
(defun read-from-abclr2-listener ()
  (unless (and *abclr2-listener* (ccl::wptr *abclr2-listener*))
    (setq *abclr2-listener* (make-instance 'abclr2-listener)))
  (unless (ccl:window-shown-p *abclr2-listener*)
    (ccl:window-show *abclr2-listener*))
  (ccl::toplevel-read))

(defun send-toplevel-order (toplevel-object message &optional reply)
  (enqueue (object-data-port toplevel-object)
           (list message reply nil)))

(defun toplevel-object-loop (Me &aux input expanded parsed translated evaled
                                error-p)
  (primary-object-loop 
   Me ((port (object-data-port Me)))
   port (M R S)
   (declare (ignore S))
   ;;
   (cond
    ((eq M :start)
     (format *terminal-io* *abclr2-prompt*)
     (force-output *terminal-io*)
     (let (#+COMMENT(*readtable* *interactive-readtable*))
       (multiple-value-setq 
         (input error-p)
         (execute-and-catch-error
          'reading-from-terminal
          #-:CCL(read *terminal-io* nil :q nil)
          #+:CCL(read-from-abclr2-listener))))
     (if (not (member input '(:q :reset)))
       (let ()
         (or 
          (progn (multiple-value-setq
                     (parsed error-p)
		   (execute-and-catch-error 
		    'parse
		    (parse input)))
		 error-p)
	  (progn (multiple-value-setq
                     (translated error-p)
		   (execute-and-catch-error
		    'translation
		    (translate parsed)))
		 error-p)
	  (progn (multiple-value-setq
                     (evaled error-p)
		   (execute-and-catch-error
		    'compilation
		    (eval translated)))
		 error-p)
	  (progn
	    (format *terminal-io*
		    "~&~S~%"
		    (prog1 
			(send-present-message 
			 (global-get '*eval* )
			 (list :do evaled *global-env*
			       (meta-of *toplevel*)
			       *group* 
			       (global-get '*eval*)) Me)
		      (run-system)))
	    ))
	 (send-toplevel-order Me :start R))
       (send-message R input nil Me)))
    ;;
    #| it seems to be used no longer.
    ((and (consp M) (eq (first M) :inc))
     (apply #'inc-file (rest M)))
    ((and (consp M) (eq (first M) :comp))
     (apply #'comp-file (rest M)))
    |#
    ;;
    (t (abclr2-warn "Toplevel object cannot handle the message ~S."
                    M)))))

;; A new toplevel object is created without process.
;; This objects is explicitly called with toplevel-object-function.
(defun create-toplevel-object (GMgr)
  (make-object-data :port (create-queue)
                    :gid GMgr
                    :type 'NRM
                    :status 'dormant
                    :name 'toplevel))

(defun startup (&key (reset t) (break-on-errors nil)
		     &aux (exit-result nil))
  #+:SBCL (setq *abclr2-error* *error-output*)
  #+:CMU (setq extensions:*gc-verbose* nil)
  (enter-lisp-error break-on-errors)
  (init-processes)
  (reset-present-message)
  (init-abclr2-readtable)
  (when (or reset (null *global-env*))
    (setq *group* (create-primary-GMgr))
    (setq *queue-gen* (create-queue-gen *group*)
          *env-gen* (create-env-gen *group*)
          *nrm-gen* (create-nrm-gen *group*))
    (setq *global-env* 
          (create-global-env 
           *group*
	   (pairlis '(env-gen queue-gen non-reifying-meta global-env
		      *eval* r2user::*library-directories*
		      r2user::*modules* r2user::*signal-handler*)
		    (list *env-gen* *queue-gen* *nrm-gen* *global-env*
			  nil *default-abclr2-library-pathnames*
			  nil nil))
           ))
    (global-set 'global-env *global-env*)
    #+COMMENT(progn
               (global-set 'env-gen *env-gen*)
               (global-set 'queue-gen *queue-gen*)
               (global-set 'non-reifying-meta *nrm-gen*)
               (global-set '*print-time* nil)
               ) ;; COMMENT
    (global-set '*eval* (send-present-message *group*
                                              :evaluator 'startup-manager))
    ;; Wed Jul 27 21:11:10 JST 1994
    (global-set 'r2user::*signal-handler*
		(create-default-signal-handler *group*)))
  (setq *toplevel* (create-toplevel-object *group*))
  
  #+:CCL
  (setq *abclr2-listener* (make-instance 'abclr2-listener))
  
  (let (#+:CCL(*terminal-io* *abclr2-listener*))
    (format *terminal-io* "~%Welcome to ~A~%~%"
            *abclr2-version*)
    (enqueue  (object-data-port *toplevel*)
              (list :start
		    (cont res (setq exit-result res))
		    'starup-manager))
    
    (in-package "ABCLR2-USER")
    
    (with-keyboard-interrupt-handler
      (with-abclr2-style-warnings-muted
        (do ()
	    ((emptyp (object-data-port *toplevel*)))
	  (toplevel-object-loop *toplevel*))))
    )
  (in-package "ABCLR2-SYSTEM")
  
  (case exit-result
    (:q					; Quit this system
     (format *terminal-io* "~&ABCL/R2 system finished.~%")
     (enter-lisp-error t)
     (values))
    (:reset				; Restart this system
     (format *terminal-io* "~&Restarting...~%")
     (startup :break-on-errors break-on-errors)))
  )
