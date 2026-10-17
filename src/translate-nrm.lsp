;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; non-reifying-meta'ed object translator
;;; Dec. 8, 1991 by MASUHARA, H.
;;; Jul. 4, 1992 time special form is added
;;; Jul.17, 1992 future type message is added.
;;; Jul.25, 1992 script with from field.
;;; Jul.27, 1992 NR-objects are named properly.
;;; Jul.28, 1992 NRM checks illegal messages
;;; Jul.29, 1992 Context sensitive inc.
;;; Aug.13, 1992 Packages in Sun-CL
;;; Jan.27, 1993 FROM field of a script of a NRM is corrected.
;;;              a NRM object now sends its denotation as a sender
;;; Jun.13, 1993 special forms reify-args are added, though they
;;; 		 cannot be used in NRM.
;;; Oct.11, 1993 minor modification for Allegro-CL
;;; Jun. 2, 1994  "         "        "  CLISP

(in-package "ABCLR2-SYSTEM")

(defun translate-NRM (exp lex-vars env-sym)
  (flet ((sub-translate (exp)
	   (translate-NRM exp lex-vars env-sym)))
    (ecase (first exp)
      (:const (const-body exp))
      (:func (func-body exp))
      (:var 
       (cond ((member (var-name exp) lex-vars) (var-name exp))
             ((eq 'Me (var-name exp)) '(den-of Me))
             ((eq 'Group (var-name exp)) '(group-of (den-of Me)))
             (t `(send-present-message ,env-sym
		  (list :value-of ',(var-name exp))
		  Me))))
      (:apply 
       (list* (apply-func exp)
              (mapcar #'sub-translate (apply-args exp))))
      (:rail
       (list* 'list (mapcar #'sub-translate (rail-body exp))))
      (:progn
	(list* 'progn (mapcar #'sub-translate (rest exp))))
      (:if
       (translate-NRM-if (if-test exp) (if-then exp) (if-else exp)
			 lex-vars env-sym))
      (:let
	  (translate-NRM-let (let-assigns exp) (let-body exp)
			     lex-vars env-sym))
      (:match
       (translate-NRM-match (match-exp exp) (match-isclauses exp)
                            (match-otherwise exp)
                            lex-vars env-sym))
      (:assign
       (translate-NRM-assign (assign-dest exp) 
                             (sub-translate (assign-exp exp))
                             lex-vars env-sym))
      (:past
       `(send-message ,(sub-translate (past-target exp))
	 ,(sub-translate (past-message exp))
	 ,(sub-translate (past-reply exp))
	 (den-of Me)))
      (:future
       (let ((reply-dest (gensym)))
       `(send-message ,(sub-translate (past-reply exp))
	 :reply-dest
	 (cont ,reply-dest
	   (send-message ,(sub-translate (past-target exp))
	     ,(sub-translate (past-message exp))
	     ,reply-dest
	    (den-of Me))))))
      (:present
       `(send-present-message ,(sub-translate (present-target exp))
	 ,(sub-translate (present-message exp))
	 (den-of Me)))
      (:object
       (translate-NRM-object (object-name exp) (object-meta-gen exp)
                             (object-group exp) (object-state-list exp)
                             (object-script-list exp)
                             lex-vars env-sym))
      (:group
       (translate-NRM-group (group-name exp)
			    (group-meta-gen exp)
			    (group-evaluator exp)
			    (group-resources exp)
			    (group-scripts exp)
			    (group-initialize exp)
			    (group-initialize-meta exp)
			    lex-vars env-sym))
      (:NRM-object
       (translate-NRM-NRM-object (object-name exp) (object-meta-gen exp)
                             (object-group exp) (object-state-list exp)
                             (object-script-list exp)
                             lex-vars env-sym))
      (:cont
       (translate-NRM-cont (cont-pattern exp) (cont-body exp)
			   lex-vars env-sym))
      (:time
       (translate-NRM-time (time-body exp) lex-vars env-sym))
      (:do
       (translate-NRM-do (do-vars exp) (do-test exp) (do-body exp)
                         lex-vars env-sym))
      (:inc
       (translate-NRM-inc (inc-args exp) lex-vars env-sym))
      (:reify-args
       (translate-NRM-reify-args (reify-args-dest exp)
                                 (reify-args-keyword exp)
                                 (reify-args-body exp)))
      )))

(defun translate-NRM-let (assigns exps lex-vars env-sym)
  (let ((inner-lex-vars
         (nconc (mapcar #'assign-dest assigns)
                lex-vars)))
    `(let ,(mapcar #'(lambda (assign)
                       (list (assign-dest assign)
                             (translate-NRM (assign-exp assign)
                                            lex-vars env-sym)))
                   assigns)
       ,@(mapcar #'(lambda (subexp)
                     (translate-NRM subexp inner-lex-vars env-sym))
                 exps))))

(defun translate-NRM-assign (dest translated-exp lex-vars env-sym)
  (cond ((member dest lex-vars)
         ;; state variable
         `(setq ,dest
                ,translated-exp))
        ((member dest '(Me Group))
         (error "Cannot assign to ~S~%while compiling ~S"
                dest translated-exp))
        (t `(send-present-message ,env-sym
              (list :set ',dest
                    ,translated-exp)
	     Me))))

(defun translate-NRM-assign-local (dest translated-exp lex-vars env-sym)
  (cond ((member dest lex-vars)
         ;; state variable
         `(setq ,dest
                ,translated-exp))
        ((member dest '(Me Group))
         (error "Cannot assign to ~S~%while compiling ~S"
                dest translated-exp))
        (t `(send-present-message ,env-sym
              (list :set-local ',dest
                    ,translated-exp)
	     Me))))

(defun translate-NRM-object (name meta-gen-spec group-spec state-list
                                  scripts lex-vars env-sym)
  (flet ((sub-translate (subexp)
                        (translate-NRM subexp lex-vars env-sym)))
    (let ((meta-gen (sub-translate meta-gen-spec))
          (group (or (sub-translate group-spec)
                     '(group-of (den-of Me))))
          (scriptSet (translate-scripts scripts))
          (stateSpec (translate-stateSpec state-list)))
      (let ((object-creating-expression
             (list 'den-of
                   (if meta-gen
                     `(send-present-message ,meta-gen
                        (list :new ,stateSpec ,env-sym ,scriptSet
                              (send-present-message ,group :evaluator Me)
                              ,group)
                        Me)
                     `(send-present-message ,group
                        (list :new ,stateSpec ,env-sym ,scriptSet)
                        Me)))))
        (if name
          (translate-NRM-assign-local name object-creating-expression
                                lex-vars env-sym)
          object-creating-expression)))))

(defun translate-NRM-if (test-exp then-exp else-exp lex-vars env-sym)
  (let ((exps (if else-exp (list test-exp then-exp else-exp)
		  (list test-exp then-exp))))
    (cons 'if
	  (mapcar #'(lambda (exp)
		(translate-NRM exp lex-vars env-sym))
	    exps))))
  
(defun translate-NRM-match (exp isclauses otherwise 
                                lex-vars env-sym)
  (flet ((sub-translate (exp)
                        (translate-NRM exp lex-vars env-sym)))
  (let ((main-expression
	 `(match
	   ,(sub-translate exp)
	   ,@(mapcar
	      #'(lambda (isclause)
		  (let ((new-lex 
			 (nconc (pickup-pattern-vars (is-pattern isclause))
				lex-vars)))
		    (list* 'is 
			   (is-pattern isclause)
			   (mapcar #'(lambda (exp)
				       (translate-NRM exp new-lex env-sym))
				   (is-exps isclause)))))
	      isclauses))))
    (if otherwise
	(append main-expression
		(list (cons 'otherwise (mapcar #'sub-translate otherwise))))
	main-expression))))

(defun pickup-pattern-vars (pattern)
  (cond ((null pattern) nil)
        ((constantp pattern) nil)
        ((symbolp pattern) (list pattern))
        ((consp pattern)
         (ecase (first pattern)
           ((list list* bracket bracket*)
            (apply #'concatenate 'list 
                   (mapcar #'pickup-pattern-vars (rest pattern))))))
        (t (error "Unkown pattern ~S" pattern))))

(defun translate-NRM-nrm-object (name meta-gen group-spec state-list
				      script-list lex-vars env-sym
				      &aux (newobj (gensym)))
  (declare (ignore meta-gen))
  (dolist (script script-list)
    (when (heavy-script-p script)
      (warn "The where field ~S for a non-reifying object is ignored."
	    (script-where script))))
  (let* ((group (or (translate-NRM group-spec lex-vars env-sym)
		    '(group-of (den-of Me))))
	 (object-creation
	  `(den-of
	    (send-present-message
		(send-present-message ,env-sym
		  (list :value-of 'non-reifying-meta) Me)
	      (list :new
		    ,(translate-NRM-state state-list lex-vars env-sym)
		    ,env-sym
		    ,(translate-NRM-script
		      script-list state-list lex-vars env-sym)
		    nil ,group)
	      Me))))
    (if name
	(translate-NRM-assign-local
	 name
	 `(let ((,newobj ,object-creation))
	   (setf (object-data-name ,newobj) ',name)
	   ,newobj)
	 lex-vars env-sym)
	object-creation)))
			      

(defun translate-NRM-state (state-list lex-vars env-sym)
  `(list '(,@(mapcar #'assign-dest state-list))
    #'(lambda (env-alist Me Group)
	,@(mapcar #'(lambda (stateSpec)
		      `(setf (cdr (pop env-alist))
                             ,(translate-NRM (assign-exp stateSpec)
                                             lex-vars env-sym)))
		  state-list))))

(defun translate-NRM-script (script-list state-list lex-vars env-sym)
  (declare (ignore env-sym))
  (let ((M-meta (gensym)) (R-meta (gensym)) (S-meta (gensym))
	(New-Env (gensym))
	(state-vars (mapcar #'assign-dest state-list)))
    `#'(lambda (Me)
	 (primary-NRM-loop 
	  Me ,New-Env ,state-vars
          (,M-meta ,R-meta ,S-meta)
	  (declare (ignore ,R-meta ,S-meta))
	  ;;
	  (if (and (consp ,M-meta)
		   (eq (first ,M-meta) :message)
		   (= 4 (length ,M-meta)))
	      (match (second ,M-meta)
		,@(mapcar #'(lambda (script)
			      (translate-NRM-script-sub
			       script (list 'third M-meta)
			       (list 'fourth M-meta)
			       (append state-vars lex-vars) New-Env))
			  script-list)
		(otherwise
		 (warn "object ~S cannot handle the message ~S"
		       (den-of Me) (second ,M-meta))))
	      (warn "Non-reifying metaobject ~S cannot handle the message ~S"
		    Me ,M-meta))))))

(defun translate-NRM-script-sub (script reply-sym from-sym lex-vars env-sym)
  (let* ((pattern-bounded
	  (let ((without-from 
		 (cons (script-reply script)
		       (nconc (pickup-pattern-vars (script-pattern script))
			      lex-vars))))
	    (if (script-from script)
		(cons (script-from script) without-from)
		without-from)))
	 (temporary-bounded
	  (nconc (mapcar #'assign-dest (script-temporary script))
		 pattern-bounded)))
    `(is ,(script-pattern script)
      (let ((,(script-reply script) ,reply-sym)
	    ,@(if (script-from script)
		  (list `(,(script-from script) ,from-sym))
		  nil))
	(let (,@(mapcar #'(lambda (assign)
			    (list (assign-dest assign)
				  (translate-NRM (assign-exp assign)
						 pattern-bounded
						 env-sym)))
		  (script-temporary script)))
				  
	,@(mapcar #'(lambda (exp)
		      (translate-NRM exp temporary-bounded env-sym))
		  (script-exps script)))))))

(defun translate-NRM-cont (pattern body lex-vars env-sym)
  (let ((new-env (nconc (pickup-pattern-vars pattern)
			lex-vars)))
  `(cont ,pattern
    ,@(mapcar #'(lambda (exp)
		  (translate-NRM exp new-env env-sym))
       body))))

(defun translate-NRM-group (name meta-gen evaluator resources
				 scripts initialize initialize-meta
				 lex-vars env-sym)
  (declare (ignore lex-vars))
  (let ((group-creation
	 `(send-present-message (group-of Me)
	   (list :new-group
	    ,(translate-stateSpec (list*
                                (list :assign 'meta-gen meta-gen)
                                (list :assign 'evaluator evaluator)
                                resources))
            ,env-sym
	    (cons (make-script
		   :initialize
                nil
		nil
                (list
                 ,@(mapcar #'translate initialize-meta)
                 (list :compiled 
                       #'(lambda (c env id gid eval)
                           (send-message eval
                             `(:do-evlis ((:variable evaluator)
                                          (:variable global-env)
                                          (:variable Me))
                                         ,env ,id ,gid ,eval)
                             (cont (list evaluator *global-env* Me)
                               (send-message eval
                                 `(:do (:send-past 
                                        ,evaluator
                                        (:do-progn 
                                         (,,@(mapcar #'translate initialize))
                                         ,*global-env* nil ,Me ,evaluator)
                                        nil)
                                       ,env ,id ,gid ,eval)
                                 c eval))
                             eval)))))
               (append ,(translate-scripts scripts)
                       *primary-GMgr-scriptSet*)))
	   Me)))
    (if name
	`(setq ,name ,group-creation)
	group-creation)))

(defun translate-NRM-time (exp lex-vars env-sym)
  `(time ,(translate-NRM exp lex-vars env-sym)))

(defconstant original-DO-symbol
    #+(or KCL (and :CCL (not :CCL-2)) :LUCID clisp) 'lisp::do ; for CLtL 1
    #+(or :CMU :CCL-2 excl :SBCL) 'common-lisp::do ; for CLtL 2
    )

(defun translate-NRM-do (vars test body lex-vars env-sym)
  (let ((inner-env (nconc (mapcar #'do-var-name vars) lex-vars)))
    `(,original-do-symbol
      ,(mapcar #'(lambda (var-decl)
                   `(,(do-var-name var-decl)
                     ,(translate-NRM (do-var-init var-decl) lex-vars env-sym)
                     ,(translate-NRM (do-var-step var-decl) inner-env env-sym)))
               vars)
      ,(mapcar #'(lambda (exp)
                   (translate-NRM exp inner-env env-sym))
               (cons (do-test-test test)
                     (do-test-results test)))
      ,@(mapcar #'(lambda (exp)
                    (translate-NRM exp inner-env env-sym))
                body))))

(defun translate-NRM-inc (args inner-env env-sym &aux (C (gensym)))
  `(send-present-message (create-io-client (group-of Me))
    #'(lambda (,C)
	(inc-main (list ,@(mapcar #'(lambda (subexp)
				      (translate-NRM subexp
						     inner-env env-sym))
				  args))
		  ,C ,env-sym Me (group-of Me)
		  (send-present-message (group-of Me)
					:evaluator Me)))
    Me))

(defun translate-NRM-reify-args (dest keyword body)
  (error "A special form ~S cannot be used in non-reifying object.~%~
          (Keyword=~S, Body=~S)"
         (ecase dest
           (:eval 'reify-args)
           (:meta 'reify-args-to-meta))
         keyword body))
