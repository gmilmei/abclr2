;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Lisp definition of primary evaluator
;;; Nov.10, 1991 by MASUHARA, H.
;;; July 17, 1992 --- future type message is added.
;;; Feb. 24, 1993 --- Changes in self-eval expression.
;;; Jun.  4, 1993 --- bug fix of reification of the evaluator.

(in-package "ABCLR2-SYSTEM")

;;
;; (create-primary-eval meta-group)
;; A primary evaluator which has meta-group as its GID is created.
;;

;; Expression sent to evaluator is parsed before sending.
(defun parse-exp (Exp) Exp)

(defun primary-eval-do-exp (Me C Exp Env Id Gid Eval)
  (let* ((exp*  (parse-exp Exp))
         (tag (and (consp exp*) (first exp*)))
         (len (and (consp exp*) (length exp*))))
    (when (numberp len) 
      ;; "len" can be substituted with "(the fixnum len)" for optimization.
      (cond
       ;;
       ;; compiled code
       ;; [:compiled script-body]
       ;;
       ((and (eq :compiled tag) (= len 2))
        (funcall (second exp*) C Env Id Gid Eval))
       ;;
       ;; variable lookup
       ;; [:variable Name]
       ;;
       ((and (eq :variable tag) (= len 2))
        (case (second exp*)
          ;; 'Me' and 'Group' are pseudo variables
          (Me (send-message C (den-of Id) nil Me))
          (Group (send-message C Gid nil Me))
          (t (send-message Env `(:value-of ,(second exp*)) C Me))))
       ;;
       ;; self evaluating
       ;; [:self-evaluate value]
       ;;
       ((and (eq :self-evaluate tag) (= len 2))
        (send-message C
	  (execute-and-catch-error 'script-evaluation
	   (funcall (second exp*))) ;; was (eval ...)
	  nil Me))
       ;;
       ;; past type message sending
       ;; [:send-past Target Message Reply (*)]
       ;; (*) extra parameters can be sent
       ((and (eq :send-past tag) (>= len 4))
        (send-message C nil nil Me)
        (if (second exp*)
          (send-message (meta-of (second exp*))
            (list* :message (third exp*) (fourth exp*) (den-of Id)
		   (nthcdr 4 exp*))
            nil Me)))
       ;;
       ;; future type message sending
       ;; [:send-future Target Message Reply (*)]
       ;; (*) extra parameters can be sent
       ((and (eq :send-future tag) (>= len 4))
        (send-message C nil nil Me)
        (if (and (second exp*) (fourth exp*))
	    (send-message (fourth exp*)
	      :reply-dest
	      (cont reply-dest
		(send-message (meta-of (second exp*))
		  (list* :message (third exp*) reply-dest (den-of Id)
			 (nthcdr 4 exp*))
		  nil Me))
              Me)))
       ;;
       ;; present (now) type message sending
       ;; [:send-now Target Message]
       ;;
       ((and (eq :send-now tag) (>= len 3))
        (if (second exp*)
          (send-message (meta-of (second exp*))
            (list* :message (third exp*) C (den-of Id)
		   (nthcdr 3 exp*))
            nil Me)))
       ;;
       ;; script object
       ;; [:script script-object]
       ;;
       ((and (eq :script tag) (= len 2))
	(call-script-body (second exp*) C Env Id Gid Eval))
       ;;
       ;; object creation
       ;; [:object-def Name Meta-gen Group State-spec Script (*)]
       ;; (*) extra parameters are sent to GMgr or meta-gen
       ((and (eq :object-def tag) )
        (let ((group-for-new (or (fourth exp*) Gid))
              (name-setter 
               (cont object
                 (if (second exp*)
                   (progn 
                     (setf (object-data-name (den-of object))
                           (second exp*))
                     (send-message Env
                       `(:set-local ,(second exp*) ,(den-of object)) C Me))
                   (send-message C (den-of object) nil Me)))))
          (if (third exp*)
            ;; Meta-gen is specified.
            (send-message group-for-new
              :evaluator
              (cont eval-for-new
                (send-message (third exp*)
                  (list* :new (fifth exp*) Env
			 (sixth exp*) eval-for-new group-for-new
			 (nthcdr 6 exp*))
                  name-setter
                  Me))
              Me)
            ;; Meta-gen is not specified, send to group manager.
            (send-message group-for-new
              (list* :new (fifth exp*) Env (sixth exp*)
		     (nthcdr 6 exp*))
              name-setter
              Me))))
       ;;
       ;; group definition
       ;; [:group-def name StateSpec Script]
       ;;
       ((and (eq :group-def tag) (= len 4))
        (send-message Gid
          (list :new-group (third exp*) Env (fourth exp*))
          (if (second exp*)
            (cont GMgr
              (setf (object-data-name GMgr) (second exp*))
              (send-message Env	; set into the environment
                `(:set-local ,(second exp*) ,GMgr) 
                C Me))
            C)
          Me))
       ;;
       ;; More definitions may be appended below.
       ;;
       (t 
        (warn "Illegal expression ~S~%" Exp)
        (if *break-illegal-message* (break)))
       ))))

(defun primary-eval-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me)))
   port (M C S)
   (declare (ignore S))
   ;;
   #+:ABCLR2-DEBUG(debug-print
                         "~S:M=~S C=~S S=~S~%" Me M C S)
   (if (and (listp M) (= 6 (length M)))
     (case (first M)
       ;;
       ;; evaluation for a single expression
       ;; [:do Exp Env Id Gid Eval]
       (:do
        (apply #'primary-eval-do-exp
               Me C (rest M)))
       ;;
       ;; sequential executions
       ;; [:do-progn Exps Env Id Gid Eval]
       (:do-progn
	   (cond ((null (second M))
		  (send-message C nil nil Me))
		 ((null (rest (second M)))
		  ;; [:do-progn [LastExp] Env Id Gid Eval]
		  (send-message (sixth M)
		    (list* :do (first (second M)) (rest (rest M)))
		    C Me))
		 (t
		  ;; [:do-progn [FirstExp . RestExp] Env Id Gid Eval]
		  (send-message (sixth M)
		    (list* :do (first (second M)) (rest (rest M)))
		    (cont _
		      (send-message (sixth M)
			(list* :do-progn (rest (second M))
			       (rest (rest M)))
			C Me))
		    Me))))
       ;;
       ;; list evaluation
       ;; [:do-evlis Exps Env Id Gid Eval]
       (:do-evlis
        (if (null (second M))
          ;; [:do-evlis NIL Env Id Gid Eval]
          (send-message C NIL nil Me)
          ;; [:do-evlis [FirstExp . RestExp] Env Id Gid Eval]
          (send-message (sixth M)
            (list* :do (first (second M))
                   (rest (rest M)))
            (cont FirstVal
              (send-message (sixth M)
                (list* :do-evlis (rest (second M))
                       (rest (rest M)))
                (cont RestVal
                  (send-message C (cons FirstVal RestVal) nil Me))
                Me))
            Me)))
       ;;
       (t
        (warn "~S cannot handle message ~S"
              Me M)
        (if *break-illegal-message* (break))))
       ;; M is not a list
       (case M
         (:reify-yourself
          (reify-eval Me (meta-of Me))
          ;; Now, Me is a Denoted object.
          (send-message C t nil Me)
          ;; (kill-process-for Me)
	  )
         (:null
          (send-message C t nil Me))
         (t
          (warn "~S cannot handle the message ~S" Me M)
          (if *break-illegal-message* (break)))))))

(defun create-primary-eval (meta-Group)
  (let ((peval (make-object-data
                :port (create-queue)
                :meta nil
                :den nil
                :gid meta-Group
                :type 'Eval
                :status 'dormant)))
    (create-process #'primary-eval-loop peval)
    peval))

(defun reify-eval (den-obj meta-obj)
  ;; data structure for new metaobject of the evaluator
  ;;  port: create a new queue
  ;;  local: queue: den-obj's port
  ;;         state: global-env ?
  ;;         scriptSet: default script for evalutaor
  ;;         evaluator: [[group-of den-obj] <== :evaluator]
  ;;         Group: [group-of meta-obj]
  ;;         mode: dormant
  ;;         send-me-end: ...
  ;;  type: Meta
  ;;  status: dormant
  (let ((metaState
         (make-primary-meta-local
          :queue	(object-data-port den-obj)
          :state	(extend-env nil *global-env*)
          :scriptSet	*primary-evaluator-script*
          :evaluator	(send-present-message (group-of den-obj)
					      :evaluator meta-obj)
          :Group	(group-of den-obj)
          :mode		':dormant
          :send-me-end  (unless use-new-primary-meta
			  (cont _ (send-message meta-obj
						:end nil meta-obj))))))
    (setf (object-data-local meta-obj) metaState)
    (let ((send-me-end (if use-new-primary-meta
			   (primary-meta-send-me-end-gen meta-obj))))
      (if use-new-primary-meta
	  (setf (primary-meta-local-send-me-end metaState) send-me-end
		(primary-meta-local-begin-proc metaState)
		  (primary-meta-begin-proc-gen
		   meta-obj (primary-meta-local-evaluator metaState)
		   (group-of den-obj)
		   *primary-evaluator-script* send-me-end)))
      (setf (object-data-port meta-obj) (create-queue)
	    (object-data-type meta-obj) 'Meta
	    (object-data-status meta-obj) 'dormant
	    (object-data-status-lock meta-obj) #+KCLM (si:make-lock)
	    				       #-KCLM nil))

    (create-process #'primary-meta-loop meta-obj)
    
    (setf (object-data-port den-obj) #'reifying-point
          (object-data-local den-obj) nil
          (object-data-type den-obj) 'Denoted
          (object-data-status den-obj) 'no-schedule
          ;; (object-data-status-lock den-obj) nil
	  )))
