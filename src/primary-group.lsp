;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Lisp definition of primary group manager
;;; Nov. 10, 1991 by MASUHARA, H.
;;; Jul. 18, 1992 --- scripts replacing group kernel objects are added.
;;; Jun.  4, 1993 --- bug fix of reification of the evaluator.
;;; Dec. 12, 1994 --- now accepts extra parameters for object creation.

(in-package "ABCLR2-SYSTEM")

;;
;; (create-primary-GMgr)
;; A primary group manager created. Initially, its meta-gen, evaluator,
;; and gid slot are empty.
;;

(defstruct GMgr-local
  meta-gen evaluator)

(defun GMgr-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me))
       (GMgr-state (object-data-local Me))
       (meta-gen (GMgr-local-meta-gen GMgr-state) :write-back)
       (evaluator (GMgr-local-evaluator GMgr-state) :write-back))
   port (M C S)
   (declare (ignore S))
   ;;
   (flet (;; lazy creation routine for meta-gen and evaluator
          (get-meta-gen ()
            (or meta-gen
                (setf meta-gen (create-primary-meta-gen (group-of Me)))))
          (get-evaluator ()
            (or evaluator
                (setf evaluator (create-primary-eval (group-of Me))))))
     (let ((len (and (consp M) (length M)))
           (tag (and (consp M) (first M))))
       (if len

         ;; M is a list.
        (cond 
         ;;
         ;; create a new metaobject
         ;; [:new StateSpec Env Script . Extra]
	 ;; Mon Dec 12 15:10:05 JST 1994 --- extra arguments cat be accepted
         ((and (eq :new tag))
          (send-message (get-meta-gen)
	    (list* (first M) (second M) (third M) (fourth M)
		   (get-evaluator) Me (nthcdr 4 M))
            C Me))
	 #+COMMENT ; original one
         ((and (eq :new tag) (= len 4))
          (send-message (get-meta-gen)
            (append M (list (get-evaluator) Me))
            C Me))
         ;;
         ;; create a new metaobject with special evaluator
         ;; [:new StateSpec Env Script Eval]
	 ;; Mon Dec 12 15:10:26 JST 1994 --- removed
	 #+COMMENT
         ((and (eq :new tag) (= len 5))
          (send-message (get-meta-gen)
            (append M (list Me))
            C Me))
         ;;
         ;; create a new group
         ;; [:new-group StateSpec Env Script]
         ((and (eq :new-group tag) (= len 4))
          (send-message (group-of Me)
            (cons :new (rest M))
            (cont meta-GMgr
              (send-message (den-of meta-GMgr)
                :initialize nil Me)
              (send-message C (den-of meta-GMgr) nil Me))
	     Me))
	 ;;
	 ;; change metaobject generator
	 ;; [:replace-meta-gen new-Meta-gen]
	 ((and (eq :replace-meta-gen tag) (= len 2))
	  (setf meta-gen (second M)))
	 ;;
	 ;; change evaluator
	 ;; [:replace-evaluator new-Eval]
	 ((and (eq :replace-evaluator tag) (= len 2))
	  (setf evaluator (second M)))
         ;;
         (t
          (warn "~S cannot handle message ~S" Me M)
          ))

        ;; M is not a list
        (case M
          ;;
          ;; ask metaobject generator
          ;;
          (:meta-gen
           (send-message C (get-meta-gen) nil Me))

          ;;
          ;; ask evaluator
          ;;
          (:evaluator
           (send-message C (get-evaluator) nil Me))

          ;;
          ;; :reify-yourself
          ;;
          (:reify-yourself
           (reify-GMgr Me (meta-of Me) (get-meta-gen) (get-evaluator))
           ;; Now, Me is Denoted-object. 
           (send-message C t nil Me)	; Notification of the completion
           ;(kill-process-for Me)	; The porcess for myself is no
           ; longer to use.
           )
          ;;
          ;; initialization
          ;;
          (:initialize
           ;; no need to initialize
           )
          ;; 
          (t
           (warn "~S cannot handle message ~S" Me M)
           ))
        )))))

(defun create-primary-GMgr ()
  (let ((GMgr (make-object-data
               :port (create-queue)
               :meta nil
               :den nil
               :gid nil
               :local (make-GMgr-local :meta-gen nil :evaluator nil)
               :type 'GMgr
               :status 'dormant)))
    (create-process #'GMgr-loop GMgr)
    GMgr))

(defun reify-GMgr (den-obj meta-obj meta-gen evaluator)
  (let ((meta-metaState			; state variables for meta-obj
	 (make-primary-meta-local
	  :queue (object-data-port den-obj)
	  :state (extend-env `((evaluator . ,evaluator)
			       (meta-gen . ,meta-gen))
			     *global-env*)
	  :scriptSet *primary-GMgr-scriptSet*
	  :evaluator (send-present-message (group-of den-obj)
					   :evaluator meta-obj)
	  :Group (group-of den-obj)
	  :mode ':dormant
	  :send-me-end
	  (unless use-new-primary-meta
	      (cont _ (send-message meta-obj :end nil meta-obj))))))
    (setf (object-data-local meta-obj) meta-metaState)
    (let ((send-me-end (if use-new-primary-meta
			   (primary-meta-send-me-end-gen meta-obj))))
      (setf (primary-meta-local-send-me-end meta-metaState) send-me-end
	    (primary-meta-local-begin-proc meta-metaState)
	      (primary-meta-begin-proc-gen
	       meta-obj (primary-meta-local-evaluator meta-metaState)
	       (group-of den-obj)
	       *primary-GMgr-scriptSet* send-me-end))
      (setf (object-data-port meta-obj) (create-queue)
	    (object-data-local meta-obj) meta-metaState
	    (object-data-type meta-obj) 'Meta
	    (object-data-status meta-obj) 'dormant
	    (object-data-status-lock meta-obj) #+KCLM (si:make-lock)
	    				       #-KCLM nil))

    (create-process #'primary-meta-loop meta-obj)

    (setf (object-data-port den-obj) #'reifying-point
	  (object-data-local den-obj) nil
	  (object-data-type den-obj) 'Denoted
	  (object-data-status den-obj) 'no-schedule)
    ))