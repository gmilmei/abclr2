;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; primary metaobject generator
;;; Nov. 10, 1991 by MASUHARA, H.
;;; Jun.  5, 1993 --- a bug that reified meta-gen won't run is fixed.
;;; Dec. 12, 1994 --- now accepts (but just ignores) extra parameters.

(in-package "ABCLR2-SYSTEM")

;;
;; (create-primary-meta-gen meta-GMgr)
;; A metaobject generator which has meta-GMgr as its GID is
;; is created.
;;

;; stateSpec is a list of consed pairs of variable name
;; and initial form.
;; e.g. (state [x := nil] [y := (+ foo 1)])
;;     --> ((x . (:self-evaluate nil))
;;          (y . (:compiled #'(lambda (...) (send-message ..)))))
(defun stateSpec-initforms (stateSpec) (mapcar #'cdr stateSpec))
(defun stateSpec-varNames (stateSpec) (mapcar #'car stateSpec))

(defun primary-meta-gen-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me)))
   port (M R S)
   (declare (ignore S))
   ;;
   (match M
     ;; Mon Dec 12 15:24:05 JST 1994 --- accept extra arguments
     (is (list* :new StateSpec LexEnv Scripts Evaluator GMgr
		Extra)
	 (if Extra (abclr2-warn "Metaobject generator ~S ignores ~
				extra arguments ~S." Me Extra))
	 ;; 1.  a new box for metaobject is created.
	 (let ((new-object (create-primary-meta
			    Scripts Evaluator GMgr (group-of GMgr))))
	   
	   ;; 2.  initial expressions are evaluated.
	   (send-message Evaluator
	     (list :do-evlis (stateSpec-initforms StateSpec)
		   LexEnv new-object GMgr Evaluator)
	     (cont initvalues
	       
	       ;; Then the evaluated values are set into the
	       ;; metaobject's data.
	       (set-metaobject-state
		new-object
                (extend-env (pairlis (stateSpec-varNames stateSpec)
                                     initvalues)
                            LexEnv))

	       ;; 3. a process is attached to the metaobject.
	       (invoke-metaobject new-object)
	       
	       ;; 4. the new metaobject is replied.
	       (send-message R new-object nil Me))
	     Me)))
     (is :reify-yourself
	 (reify-meta-gen Me (meta-of Me))
	 ;;
	 (send-message R t nil Me)
	 )

     (otherwise
      (warn "~S cannot handle message ~S" Me M)))))

(defun create-primary-meta-gen (meta-GMgr)
  (let ((meta-gen (make-object-data :port (create-queue)
                                    :meta nil
                                    :den nil
                                    :gid meta-GMgr
                                    :type 'meta-gen
                                    :status 'dormant)))
    (create-process #'primary-meta-gen-loop meta-gen)
    meta-gen))

(defun reify-meta-gen (den-obj meta-obj)
  (let ((meta-metaState
	 (make-primary-meta-local
	  :queue	(object-data-port den-obj)
	  :state	(extend-env nil *global-env*)
	  :scriptSet	*primary-meta-gen-scriptset*
	  :evaluator	(send-present-message (group-of den-obj)
					      :evaluator meta-obj)
	  :Group	(group-of den-obj)
	  :mode		':dormant
	  :send-me-end	(unless use-new-primary-meta
			  (cont _ (send-message meta-obj
						:end nil meta-obj))))))
    (setf (object-data-local meta-obj) meta-metaState)
    (when use-new-primary-meta
      (let ((send-me-end (if use-new-primary-meta
			     (primary-meta-send-me-end-gen meta-obj))))
	(setf (primary-meta-local-send-me-end meta-metaState) send-me-end
	      (primary-meta-local-begin-proc meta-metaState)
	      (primary-meta-begin-proc-gen
	       meta-obj (primary-meta-local-evaluator meta-metaState)
	       (group-of den-obj)
	       *primary-meta-gen-scriptset* send-me-end))))
    (setf (object-data-port meta-obj) (create-queue)
	  (object-data-type meta-obj) 'Meta
	  (object-data-status meta-obj) 'dormant
	  (object-data-status-lock meta-obj) #+KCLM (si:make-lock)
	                                     #-KCLM nil)

    (create-process #'primary-meta-loop meta-obj)

    (setf (object-data-port den-obj) #'reifying-point
	  (object-data-local den-obj) nil
	  (object-data-type den-obj) 'Denoted
	  (object-data-status den-obj) 'no-schedule
	  ;; (object-data-status-lock den-obj) nil
	  )))
	  