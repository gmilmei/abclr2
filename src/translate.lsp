;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; script compiler
;;; Dec. 2, 1991 by MASUHARA, H.
;;; Apr.19, 1992 --- special form time is added.
;;; Jul.17, 1992 --- uncommon error of [cont ...] form is fixed.
;;;		 --- future type message is added.
;;; Jul.25, 1992 --- script with FROM field.
;;; Jul.27, 1992 --- script with WHERE field.
;;; Jul.29, 1992 --- context sensitive inc.
;;; Aug. 1, 1992 --- minor bug fix.
;;; Jan.23, 1993 --- (let ((x (progn 1))) x) now returns 1.
;;; Feb.24, 1993 --- changes in self-eval type expressions
;;; Jun.13, 1993 --- special forms reify-args are added.
;;; May.11, 1994 --- backquotes are no longer used to compose messages
;;; Dec.14, 1994 --- where fields are separated from bodies

(in-package "ABCLR2-SYSTEM")

(defconstant constraint-fail-message :constraint-fail
  "A message sent to a metaobject when the test of WHERE-field is failed.")
(defconstant heavy-meta-name 'heavy-meta
  "Name of metaobject generator for objects with where-field script.")

(defun self-eval-p (translated-exp)
  (or (eq ':self-evaluate (second translated-exp))
      (and (eq 'quote (first translated-exp))
           (eq ':self-evaluate (first (second translated-exp))))))

(defun self-eval-body (translated-exp)
  (if (eq ':self-evaluate (second translated-exp))
    (third translated-exp)
    (list 'quote (second (second translated-exp)))))

(defun unquote (exp)
  (list 'funcall exp)
  #+COMMENT`(funcall ,exp)
  #+COMMENT ;; when we were EVALuating :SELF-EVAL type expressions....
  (if (and (consp exp) (eq 'quote (first exp)))
      (second exp)
      exp))

(defun with-eval (translated-exps eval-params body-producer)
  (let* ((tagged-exps
            (mapcar #'(lambda (exp) (cons (gensym) exp))
		    translated-exps))
	 (constants
	    (remove-if-not
	     #'(lambda (sym-exp) (self-eval-p (cdr sym-exp)))
	     tagged-exps))
	 (non-constants
	    (remove-if
	     #'(lambda (sym-exp) (self-eval-p (cdr sym-exp)))
	     tagged-exps)))
    (cond ((null non-constants)
	   (null-eval-form translated-exps eval-params body-producer))
	  ((null (rest non-constants))
	   (one-eval-form (mapcar #'car tagged-exps)
	    (first non-constants) constants eval-params
	    body-producer))
	  (t
	   (multi-eval-form
	    (mapcar #'car tagged-exps)
	    non-constants constants eval-params
	    body-producer)))))

(defun null-eval-form (constants eval-params body-producer)
  `(list :compiled
    #'(lambda ,eval-params
	,@(funcall body-producer
		   (mapcar #'(lambda (const)
                               ;; Below, a function unquote is
                               ;; inserted for produce a correct code
                               ;; for (let ((x (progn 1))) x).
			       (unquote (self-eval-body const)))
			   constants)))))

(defun one-eval-form (symbol-order non-constant constants eval-params
                                   body-producer &aux (tmp (gensym)))
  `(list :compiled
         #'(lambda ,eval-params
             (send-message ,(fifth eval-params)
               (list :do ,(rest non-constant) ,@(rest eval-params))
               #+COMMENT`(:do ,,(rest non-constant) ,,@(rest eval-params))
               (cont ,tmp
                 ,@(funcall body-producer
                            (mapcar #'(lambda (symbol
                                               &aux (p (assoc symbol constants)))
                                        (if (eq symbol (first non-constant))
                                          tmp
                                          (unquote (self-eval-body (rest p)))))
                                    symbol-order)))
               ,(fifth eval-params)))))

(defun multi-eval-form (symbol-order non-constants constants eval-params
				     body-producer)
  `(list :compiled
    #'(lambda ,eval-params
	(send-message ,(fifth eval-params)
	  (list :do-evlis
		(list ,@(mapcar #'(lambda (non-constant)
					  (rest non-constant))
				      non-constants))
		,@(rest eval-params))
          #+COMMENT
	  `(:do-evlis
	    ( ,,@(mapcar #'(lambda (non-constant)
			     (rest non-constant))
		  non-constants))
	    ,,@(rest eval-params))
	  (cont (list ,@(mapcar #'(lambda (non-constant)
				(first non-constant))
			    non-constants))
	    ,@(funcall body-producer
		       (mapcar
			#'(lambda (symbol
				   &aux (pc (assoc symbol constants)))
				    
			    (if pc
                              (unquote (self-eval-body (rest pc)))
				(first (assoc symbol non-constants))))
			symbol-order)))
	  ,(fifth eval-params)))))

;;
(defun with-eval-vars (variables eval-params body-producer)
  (let ((var-set (remove-duplicates variables)))
    (with-eval (mapcar #'(lambda (var)
			   `'(:variable ,var))
		       var-set)
      eval-params
      #'(lambda (vars-tmp)
	  (funcall body-producer
		   (pairlis var-set vars-tmp))))))
			    
;;

(defun functional-p (pexp)
  (case (first pexp)
    ((:var :const :func) t)
    (:apply
     (every #'functional-p (apply-args pexp)))
    (:rail
     (every #'functional-p (rail-body pexp)))
    (t nil)))

;; A parsed expression can be translated into two forms.
;; The one is a form which can be embedded into other expressions,
;; another is a form which can be sent to evaluator with :do.

;; A embedded form consists of a form creator and referece variable list.
;; The form creator is a function which is called with substitution alist,
;; and produces a form.
(defun translate-embedded (pexp)
  (case (first pexp)
    (:var
     (list #'(lambda (subst)
	       (cdr (assoc (var-name pexp) subst)))
	   (list (var-name pexp))))
    (:const
     (list #'(lambda (subst)
	       (declare (ignore subst))
	       (const-body pexp))
	   nil))
    (:func
     (list #'(lambda (subst) (declare (ignore subst))
              (func-body pexp))
           nil))
    (:apply
     (let* ((translated-args (mapcar #'translate-embedded
				    (apply-args pexp)))
	    (embedded-args (mapcar #'first translated-args))
	    (ref-vars (apply #'concatenate 'list
			     (mapcar #'second translated-args))))
				   
       (list #'(lambda (subst)
		 (cons (apply-func pexp)
		       (mapcar #'(lambda (arg-producer)
				   (funcall arg-producer subst))
			       embedded-args)))
	     ref-vars)))
    (:rail
     (let* ((translated-body (mapcar #'translate-embedded
				     (rail-body pexp)))
	    (embedded-body (mapcar #'first translated-body))
	    (ref-vars (apply #'concatenate 'list
			     (mapcar #'second translated-body))))
       (list #'(lambda (subst)
		 (cons 'list
		       (mapcar #'(lambda (body-producer)
				   (funcall body-producer subst))
			       embedded-body)))
	     ref-vars)))
    (t (error "~S cannot be turned into embedded form."
	      pexp))))

;; evaluatable form can be sent to evaluator with ':do.
(defun translate (exp) ; exp is parsed expression
  (case (first exp)
    (:const `(list :self-evaluate #'(lambda () ,(const-body exp))))
    #+COMMENT (:const `'(:self-evaluate ,(const-body exp)))
    (:func `(list :self-evaluate #'(lambda () ,(func-body exp))))
    #+COMMENT (:func `'(:self-evaluate ,(func-body exp)))
    (:var `'(:variable ,(var-name exp)))
    (:assign (translate-assign
              (assign-dest exp) (assign-exp exp) (assign-extra exp)))
    (:present (translate-present
               (present-target exp) (present-message exp) 
               (present-extra exp)))
    (:past (translate-past
            (past-target exp) (past-message exp) 
            (past-reply exp) (past-extra exp)))
    (:future (translate-future
            (past-target exp) (past-message exp) 
            (past-reply exp) (past-extra exp)))
    (:apply (translate-apply (apply-func exp) (apply-args exp)))
    (:rail (translate-rail (rail-body exp)))
    (:object (translate-object (object-name exp)
				      (object-meta-gen exp)
				      (object-group exp)
				      (object-state-list exp)
				      (object-script-list exp)
				      (object-extra exp)))
    (:NRM-object
     (translate-object-NRM (object-name exp)
			   (object-meta-gen exp)
			   (object-group exp)
			   (object-state-list exp)
			   (object-script-list exp)))
    (:group
     (translate-group (group-name exp)
		      (group-meta-gen exp)
		      (group-evaluator exp)
		      (group-resources exp)
		      (group-scripts exp)
		      (group-initialize exp)
		      (group-initialize-meta exp)))
    (:if (translate-if (if-test exp) (if-then exp) (if-else exp)))
    (:progn (translate-progn (rest exp)))
    (:let (second (translate-temporary (let-assigns exp) (let-body exp))))
    #+COMMENT
    (:composed
     (translate-composed (composed-producer exp) (composed-varlist exp)))

    (:match
     (translate-match
      (match-exp exp) (match-isclauses exp) (match-otherwise exp)))
    (:script ;; script as a frist class object
     (translate-user-script exp))
    (:cont
     (translate-cont (cont-pattern exp) (cont-body exp)))
    (:time
     (translate-time (time-body exp)))
    (:do
     (translate-do (do-vars exp) (do-test exp) (do-body exp)))
    (:inc
     (translate-inc (inc-args exp)))
    (:reify-args
     (translate-reify-args (reify-args-dest exp) (reify-args-keyword exp)
                           (reify-args-body exp)))
    (t (error "~S is undefined" exp))))

;; translations for each form
(defun translate-present (target message extra)
  (if (every #'functional-p (list* target message extra))
      ;; all arguments are functional
      (let* ((translated-args
	      (mapcar #'translate-embedded (list* target message extra))))
	(with-eval-vars
	    (apply #'concatenate 'list (mapcar #'second translated-args))
	  '(c env id gid eval)
	  #'(lambda (subst)
	      `((send-message eval
		 (list :do (list :send-now
				 ,@(mapcar #'(lambda (arg)
					       (funcall arg subst))
					   (mapcar #'first translated-args)))
		   env id gid eval)
		 #+COMMENT
		 `(:do (:send-now ,,@(mapcar #'(lambda (arg)
						 (funcall arg subst))
				      (mapcar #'first translated-args)))
		   ,env ,id ,gid ,eval)
		 c eval)))))
      ;; some argument is not functional
      (with-eval (mapcar #'translate (list* target message extra))
	'(c env id gid eval)
	#'(lambda (args)
	    `((send-message eval
	       (list :do (list :send-now ,@args) env id gid eval)
	       #+COMMENT
	       `(:do (:send-now ,,@args) ,env ,id ,gid ,eval)
	       c eval))))))

(defun translate-past (target message reply extra)
  (if (every #'functional-p (list* target message reply extra))
      ;; all arguments are functional
      (let* ((translated-args
	      (mapcar #'translate-embedded
		      (list* target message reply extra))))
	(with-eval-vars
	    (apply #'concatenate 'list (mapcar #'second translated-args))
	  '(c env id gid eval)
	  #'(lambda (subst)
	      `((send-message eval
		 (list :do (list :send-past
				 ,@(mapcar #'(lambda (arg)
					       (funcall arg subst))
					   (mapcar #'first translated-args)))
		  env id gid eval)
		 #+COMMENT
		 `(:do (:send-past ,,@(mapcar #'(lambda (arg)
						 (funcall arg subst))
				      (mapcar #'first translated-args)))
		   ,env ,id ,gid ,eval)
		 c eval)))))
      ;; some argument is not functional
      (with-eval (mapcar #'translate (list* target message reply extra))
	'(c env id gid eval)
	#'(lambda (args)
	    `((send-message eval
	       (list :do (list :send-past ,@args) env id gid eval)
	       #+COMMENT
	       `(:do (:send-past ,,@args) ,env ,id ,gid ,eval)
	       c eval))))))

;; added on July 17, 1992
(defun translate-future (target message reply extra)
  (if (every #'functional-p (list* target message reply extra))
      ;; all arguments are functional
      (let* ((translated-args
	      (mapcar #'translate-embedded
		      (list* target message reply extra))))
	(with-eval-vars
	    (apply #'concatenate 'list (mapcar #'second translated-args))
	  '(c env id gid eval)
	  #'(lambda (subst)
	      `((send-message eval
		 (list :do (list :send-future
				 ,@(mapcar #'(lambda (arg)
					       (funcall arg subst))
					   (mapcar #'first translated-args)))
		  env id gid eval)
		 #+COMMENT
		 `(:do (:send-future ,,@(mapcar #'(lambda (arg)
						 (funcall arg subst))
				      (mapcar #'first translated-args)))
		   ,env ,id ,gid ,eval)
		 c eval)))))
      ;; some argument is not functional
      (with-eval (mapcar #'translate (list* target message reply extra))
	'(c env id gid eval)
	#'(lambda (args)
	    `((send-message eval
	       (list :do (list :send-future ,@args) env id gid eval)
	       c eval))))))

(defun translate-assign (name exp extra)
  (if (every #'functional-p (list* exp extra))
      ;; all arguments are functional
      (let* ((translated-args
	      (mapcar #'translate-embedded
		      (list* exp extra))))
	(with-eval-vars
	    (apply #'concatenate 'list (mapcar #'second translated-args))
	  '(c env id gid eval)
	  #'(lambda (subst)
	      `((send-message env
		 (list :set ',name
		  ,@(mapcar #'(lambda (arg)
				(funcall arg subst))
			    (mapcar #'first translated-args)))
		 c eval)))))
      ;; some argument is not functional
      (with-eval (mapcar #'translate (list* exp extra))
	'(c env id gid eval)
	#'(lambda (args)
	    `((send-message env
	       (list :set ',name ,@args)
	       c eval))))))

(defun translate-apply (func-name args)
  (if (every #'functional-p args)
    ;; all arguments are functional
    (let* ((translated-args
            (mapcar #'translate-embedded args))
           (varlist (apply #'concatenate
                           'list (mapcar #'second translated-args))))
      (if varlist
        ;; some variables are needed to be eval'ed.
        (with-eval-vars
          varlist
          '(c env id gid eval)
          #'(lambda (subst)
              `((send-message c 
                  (,func-name ,@(mapcar #'(lambda (arg)
                                            (funcall arg subst))
                                        (mapcar #'first translated-args)))
                  nil eval))))
        ;; no need to evaluate variales
        `(list :self-evaluate
	  #'(lambda ()
	      (,func-name ,@(mapcar #'(lambda (arg)
					(funcall arg nil))
				    (mapcar #'first translated-args))))
	  #+COMMENT
               '(,func-name ,@(mapcar #'(lambda (arg)
                                          (funcall arg nil))
                                      (mapcar #'first translated-args))))))
    ;; some argument is not functional
    (with-eval (mapcar #'translate args)
      '(c env id gid eval)
      #'(lambda (args)
          `((send-message c
              (,func-name ,@args)
              c eval))))))

(defun translate-rail (body)
  (if (every #'functional-p body)
      ;; all arguments are functional
      (let* ((translated-args
	      (mapcar #'translate-embedded body)))
	(with-eval-vars
	    (apply #'concatenate 'list (mapcar #'second translated-args))
	  '(c env id gid eval)
	  #'(lambda (subst)
	      `((send-message c 
		 (list ,@(mapcar #'(lambda (arg)
					   (funcall arg subst))
				       (mapcar #'first translated-args)))
		 nil eval)))))
      ;; some argument is not functional
      `(list :compiled
	#'(lambda (c env id gid eval)
	    (send-message eval
	      (list :do-evlis (list ,@(mapcar #'translate body))
		    env id gid eval)
              #+COMMENT
	      `(:do-evlis (,,@(mapcar #'translate body))
		,env ,id ,gid ,eval)
	      c eval)))))

(defun heavy-script-p (script) (not (null (script-where script))))

(defun translate-object (name meta-gen group state scripts extra
			      &aux (object-weight :normal))
  ;; When one ore more scripts have heavy specification (i.e., using
  ;; WHERE form), this object should be compiled as heavy-object.
  ;; The default metaobject generator of a heavy object is "heavy-meta".
  (when (some #'heavy-script-p scripts)
    (setq object-weight ':heavy)
    (when (and (equal meta-gen '(:const nil))
	       (equal group '(:const nil)))
      (setq meta-gen (list ':var heavy-meta-name))))
  ;;
  (let ((specs (list* meta-gen group
		      (mapcar #'second extra))))
    (if (every #'functional-p specs)
	;; all arguments are functional
	(let* ((translated (mapcar #'translate-embedded specs))
	       (ref-vars (apply #'concatenate 'list
				(mapcar #'second translated))))
	  (if ref-vars
	      ;; this definition requires some evaluation before creation
	      (with-eval-vars ref-vars
		'(c env id gid eval)
		#'(lambda (subst)
		    `((send-message eval
		       (list :do
			 (list
			  :object-def ',name
			  ,(funcall (first (first translated)) subst)
			  ,(funcall (first (second translated)) subst)
			  ,(translate-stateSpec state)
			  ,(translate-scripts scripts :weight
					      object-weight)
			  ,@(apply #'concatenate 'list
				   (mapcar
				    #'(lambda (untranslated-exp
					       producer)
					(list
					 (first untranslated-exp)
					 (funcall (first producer) subst)))
					   extra (rest (rest translated)))))
			env id gid eval)
		       #+COMMENT
		       `(:do
			 (:object-def ,',name
			  ,,(funcall (first (first translated)) subst)
			  ,,(funcall (first (second translated)) subst)
			  ,,(translate-stateSpec state)
			  ,,(translate-scripts scripts :weight object-weight)
			  ,,@(apply #'concatenate 'list
			      (mapcar #'(lambda (untranslated-exp
						producer)
					 (list
					   (first untranslated-exp)
					   (funcall (first producer) subst)))
			      extra (rest (rest translated)))))
			 ,env ,id ,gid ,eval)
		       c eval))))
	      ;; meta-gen spec is constant
	      `(list :object-def ',name
		,(funcall (first (first translated)) nil)
		,(funcall (first (second translated)) nil)
		,(translate-stateSpec state)
		,(translate-scripts scripts :weight object-weight)
		,@(apply #'concatenate 'list
			      (mapcar #'(lambda (untranslated-exp
						producer)
					 (list
					   (first untranslated-exp)
					   (funcall (first producer) nil)))
			      extra (rest (rest translated)))))
	      ))
	;; some argument is not functional
	(with-eval (mapcar #'translate specs)
	  '(c env id gid eval)
	  #'(lambda (specs*)
	      `((send-message eval
		 (list :do
		  (list :object-def ,name
			,(first specs*)
			,(second specs*)
			,(translate-stateSpec state)
			,(translate-scripts scripts :weight object-weight)
			,@(apply #'concatenate 'list
				 (mapcar #'(lambda (untranslated-exp sym)
					     (list (first untranslated-exp)
						   sym))
					 extra (rest (rest specs*)))))
		  env id gid eval)
		 #+COMMENT
		 `(do (:object-def ,,name
		       ,,(first specs*)
		       ,,(second specs*)
		       ,,(translate-stateSpec state)
		       ,,(translate-scripts scripts :weight object-weight)
		       ,,@(apply #'concatenate 'list
			   (mapcar #'(lambda (untranslated-exp sym)
				       (list (first untranslated-exp)
					sym))
			   extra (rest (rest specs*)))))
		   ,env ,id ,gid ,eval)
		 c eval)))))))

(defun translate-stateSpec (state-list)
  `(list ,@(mapcar #'(lambda (sym exp)
		       (list 'cons (list 'quote sym) exp)
		       #+COMMENT
		       `(cons ',sym ,exp))
	    (mapcar #'assign-dest state-list)
	    (mapcar #'(lambda (assign)
			(translate (assign-exp assign)))
	     state-list))))

(defun translate-scripts (script-list &rest keywords)
  `(list ,@(mapcar #'(lambda (script)
		       (apply #'translate-script script
			      keywords))
	    script-list)))

;;; Wed Dec 14 15:31:06 JST 1994
;;; Script objects now can have where-exp field.
(defun translate-script (script &key weight)
  (declare (ignore weight))
  (let ((reply (script-reply script)) (from (script-from script))
	(where (script-where script)) (temporary (script-temporary script))
	(exps (script-exps script)))
    `(make-script
      ,(script-pattern script)
      ,(and reply `(quote ,reply))
      ,(and from `(quote ,from))
      ,(translate-temporary temporary exps)
      nil ;; GID
      ,(and where (translate where)))))

;; the original
#+COMMENT
(defun translate-script (script &key weight)
  (declare (ignore weight))
  (let ((reply (script-reply script)) (from (script-from script))
	(where (script-where script)) (temporary (script-temporary script))
	(exps (script-exps script)))
    `(make-script
      ,(script-pattern script)
      ,(and reply `(quote ,reply))
      ,(and from `(quote ,from))
      ,(cond ((and (null where) (null temporary))
	      `(list ,@(mapcar #'translate exps)))
	     ((not (null where))
	      (translate-script/where where temporary exps))
	     (t
	      (translate-temporary temporary exps))))))

;;; This function is no longer called because where guard are no longer
;;; embedded in a script's body.
(defun translate-script/where (where-form temporary exps)
  `(list :compiled
     #'(lambda (C Env Id Gid Eval)
	 (send-message Eval
	   (list :do ,(translate where-form) Env Id Gid Eval)
	   (cont Result
	     (if Result
		 ;; constraint holds
		 (send-message Eval
		   (list :do-progn
			 ,(if temporary
			      (translate-temporary temporary exps)
			      `(list ,@(mapcar #'translate exps)))
			 Env Id Gid Eval)
		   C Eval)
		 ;; constraint fails
		 (send-message Id ,constraint-fail-message nil Eval)))
           Eval))))
		 
(defun translate-temporary (assignments exps)
  (let ((assignment-variables (mapcar #'assign-dest assignments))
        (assignment-exps (mapcar #'assign-exp assignments)))
    (if (every #'functional-p assignment-exps)
      (let* ((translated
              (mapcar #'translate-embedded assignment-exps))
             (ref-vars (apply #'concatenate 'list
                              (mapcar #'second translated))))
        (if ref-vars
          `(list
            ,(with-eval-vars ref-vars
               '(c env id gid eval)
               #'(lambda (subst)
                   (list ;; 'list ???
                         (translate-send-progn-or-do
                          exps
                          'c
                          `(extend-env 
                            (pairlis ',assignment-variables
                                     (list ,@(mapcar #'(lambda (exp)
                                                         (funcall (first exp) subst))
                                                     translated)))
                            env)
                          'id 'gid 'eval)))))
          `(list 
            (list :compiled
                  #'(lambda (c env id gid eval)
                      ,(translate-send-progn-or-do
                        exps
                        'c
                        `(extend-env 
                          (pairlis ',assignment-variables
                                   (list ,@(mapcar #'(lambda (exp)
                                                       (funcall (first exp) nil))
                                                   translated)))
                          env)
                        'id 'gid 'eval))))))
      `(list
        ,(with-eval (mapcar #'(lambda (exp)
                                (translate (assign-exp exp)))
                            assignments)
           '(c env id gid eval)
           #'(lambda (values)
               (list (translate-send-progn-or-do 
                      exps
                      'c `(extend-env (pairlis ',(mapcar #'assign-dest assignments)
                                               (list ,@values))
                                      env)
                      'id 'gid 'eval)))
           )))))

(defun translate-object-NRM (name meta-gen group state scripts)
  (dolist (script scripts)
    (when (heavy-script-p script)
      (warn "The where field ~S for a non-reifying object is ignored."
	    (script-where script))))
  (if (functional-p group)
      (let* ((translated
	      (list (translate-embedded meta-gen)
		    (translate-embedded group)))
	     (ref-vars (apply #'concatenate 'list
			      (mapcar #'second translated))))
		(if ref-vars
	    ;; this definition requires some evaluation before creation
	    (with-eval-vars ref-vars
	      '(c env id gid eval)
	      #'(lambda (subst)
		  `((send-message eval
		      (list :do
		       (list :object-def ',name
			     ,(funcall (first (first translated)) subst)
			     ,(funcall (first (second translated)) subst)
			     ,(translate-NRM-state state nil 'env)
			     ,(translate-NRM-script scripts state nil 'env))
			env id gid eval)
		     #+COMMENT
		      `(:do (:object-def ,',name
			     ,,(funcall (first (first translated)) subst)
			     ,,(funcall (first (second translated)) subst)
			     ,,(translate-NRM-state state nil 'env)
			     ,,(translate-NRM-script scripts state nil 'env))
			,env ,id ,gid ,eval)
		     c eval))))
	    ;; meta-gen spec is constant
	    `(list :object-def ',name
	      ,(funcall (first (first translated)) nil)
	      ,(funcall (first (second translated)) nil)
	      ,(translate-NRM-state state nil 'env)
	      ,(translate-NRM-script scripts state nil 'env))
	    ))
	;; some argument is not functional
	(with-eval (mapcar #'translate (list meta-gen group))
	  '(c env id gid eval)
	  #'(lambda (meta-gen-and-group)
	      `((send-message eval
		 (list :do
		  (list :object-def ,name
			,(first meta-gen-and-group)
			,(second meta-gen-and-group)
			,(translate-NRM-state state nil 'env)
			,(translate-NRM-script scripts state nil 'env))
		  env id gid eval)
		 #+COMMENT
		 `(do (:object-def ,,name
			,,(first meta-gen-and-group)
			,,(second meta-gen-and-group)
			,,(translate-NRM-state state nil 'env)
			,,(translate-NRM-script scripts state nil 'env))
		    ,env ,id ,gid ,eval)
		  c eval))))))


(defun translate-group (name meta-gen evaluator resources
                             script initialize initialize-meta)
  (let ((translated-initialize
	 (and initialize 
	      (list 
	       `(list :compiled 
		 #'(lambda (c env id gid eval)
		     (send-message eval
		       (list :do-evlis
			     '((:variable evaluator)
			       (:variable global-env)
			       (:variable Me))
			 env id gid eval)
		       #+COMMENT
		       `(:do-evlis ((:variable evaluator)
				    (:variable global-env)
				    (:variable Me))
			 ,env ,id ,gid ,eval)
		       (cont (list evaluator *global-env* Me)
			 (send-message eval
			   (list :do
			    (list :send-past 
				  evaluator
				  (list :do-progn 
					(list
					 ,@(mapcar #'translate initialize))
				    *global-env* nil Me evaluator)
				  nil)
			     env id gid eval)
			   #+COMMENT
			   `(:do (:send-past 
				  ,evaluator
				  (:do-progn 
				      (,,@(mapcar #'translate initialize))
				    ,*global-env* nil ,Me ,evaluator)
				  nil)
			     ,env ,id ,gid ,eval)
			   c eval))
		       eval))))))
	(translated-initialize-meta
	 (and initialize-meta
	      (mapcar #'translate initialize-meta))))
    `(list :group-def ',name
      ;; compose meta-gen, evaluator and user-defined resources
      ;; as a state variables
      ,(translate-stateSpec (list*
			     (list :assign 'meta-gen meta-gen)
			     (list :assign 'evaluator evaluator)
			     resources))
      ;; 
      (cons (make-script
	     :initialize
	     nil
	     nil
	     (append
	      ,(concatenate 'list
			    translated-initialize-meta
			    translated-initialize)))
       (append ,(translate-scripts script)
	*primary-GMgr-scriptSet*))
      )))

(defun translate-if (test-exp then-exp else-exp)
  (unless else-exp (setq else-exp '(:const nil)))
  (let* ((functional-then (functional-p then-exp))
	 (embedded-then (and functional-then
			     (translate-embedded then-exp)))
	 (then-form
	  (if (and functional-then
		   (null (second embedded-then)))
	      `(send-message c
		,(funcall (first embedded-then) nil) nil eval)
	      `(send-message eval
		(list :do ,(translate then-exp) env id gid eval)
		#+COMMENT
		`(:do ,,(translate then-exp) ,env ,id ,gid ,eval)
		c eval)))
	 (functional-else (functional-p else-exp))
	 (embedded-else (and functional-else
			     (translate-embedded else-exp)))
	 (else-form
	  (if (and functional-else
		   (null (second embedded-else)))
	      `(send-message c
		,(funcall (first embedded-else) nil) nil eval)
	      `(send-message eval
		(list :do ,(translate else-exp) env id gid eval)
		#+COMMENT
		`(:do ,,(translate else-exp) ,env ,id ,gid ,eval)
		c eval))))
    (with-eval (list (translate test-exp))
      '(c env id gid eval)
      #'(lambda (test)
	  `((if ,(first test)
		,then-form ,else-form))))))

(defun translate-progn (exps)
  (if (every #'functional-p exps)
    ;; all arguments are functional
    (let* ((translated-args
            (mapcar #'translate-embedded exps))
           (ref-vars
            (apply #'concatenate 'list (mapcar #'second translated-args))))
      (if (null ref-vars)
        `(list :self-evaluate
	  #'(lambda () (progn ,@(mapcar #'(lambda (arg)
					    (funcall arg nil))
					(mapcar #'first translated-args))))
	  #+COMMENT '(progn ,@(mapcar #'(lambda (arg)
					  (funcall arg nil))
			       (mapcar #'first translated-args))))
        (with-eval-vars
          ref-vars
          '(c env id gid eval)
          #'(lambda (subst)
              `((send-message c
                  (progn ,@(mapcar #'(lambda (arg)
                                       (funcall arg subst))
                                   (mapcar #'first translated-args)))
                  
                  c eval))))))
    ;; some argument is not functional
    `(list :compiled
           #'(lambda (c env id gid eval)
               (send-message eval
                 (list :do-progn (list ,@(mapcar #'translate exps))
		       env id gid eval)
		 #+COMMENT
                 `(:do-progn (,,@(mapcar #'translate exps))
                             ,env ,id ,gid ,eval)
                 c eval)))))

(defun translate-match (exp isclauses otherwise)
  (if (functional-p exp)
    (let* ((translated (translate-embedded exp))
           (ref-vars (second translated)))
      (with-eval-vars ref-vars
        '(c env id gid eval)
        #'(lambda (subst)
            `((let ((value ,(funcall (first translated) subst))
                    bindings)
                (cond 
                 ,@(mapcar #'(lambda (isclause)
                               (translate-isclause isclause 'value))
                           isclauses)
                 (t ,(translate-send-progn-or-do 
                      otherwise
                      'c 'env 'id 'gid 'eval))))))))
    (with-eval (list (translate exp))
      '(c env id gid eval)
      #'(lambda (val)
          `((let ((value ,(first val)) bindings)
              (cond 
               ,@(mapcar #'(lambda (isclause)
                             (translate-isclause isclause 'value))
                         isclauses)
               (t 
                ,(translate-send-progn-or-do 
                  otherwise
                  'c 'env 'id 'gid 'eval)
                ))))))))

#+COMMENT
(defun translate-isclause (isclause val &aux
                                    (exps (is-exps isclause))
                                    (pattern (is-pattern isclause)))
  (if (every #'functional-p exps)
    (let* ((translated (mapcar #'translate-embedded exps))
           (ref-vars (apply #'concatenate 'list 
                            (mapcar #'second translated))))
      (if ref-vars
        (let ((syms (mapcar #'(lambda (var) 
                                (cons var (gensym)))
                            ref-vars)))
        `((setq bindings 
                (funcall (make-script-binding-function ,pattern)
                         ,val))
          (send-message eval
            (list :do-evlis
                  (list ,@(mapcar #'(lambda (var) `'(:variable ,var))
                                  ref-vars))
                  (extend-env bindings env)
                  id gid eval)
            (cont (list ,@(mapcar #'cdr syms))
              (send-message c 
                (progn ,@(mapcar 
                          #'(lambda (exp) (funcall (first exp) syms))
                          translated))
                nil eval))
            eval)))
        `((setq bindings 
                (funcall (make-script-binding-function ,pattern)
                         ,val))
          (send-message c
           (progn ,@(mapcar #'(lambda (exp)
                                (funcall (first exp) nil))
                            translated))
           nil eval))))
    `((setq bindings 
                (funcall (make-script-binding-function ,pattern)
                         ,val))
      (send-message eval
       (list :do-progn (list ,@(mapcar #'translate exps))
	env id gid eval)
       #+COMMENT
       `(:do-progn (,,@(mapcar #'translate exps))
                   ,env ,id ,gid ,eval)
       c eval))))

(defun translate-isclause (isclause val &aux
                                    (exps (is-exps isclause))
                                    (pattern (is-pattern isclause)))
  `((setq bindings 
                (funcall (make-script-binding-function ,pattern)
                         ,val))
    ,(translate-send-progn-or-do exps 
                                 'c
                                 `(extend-env bindings env)
                                 'id 'gid 'eval)))
#+COMMENT
(translate-send-progn-or-do
      (mapcar #'translate (is-exps isclause))
      'c `(send-present-message *env-gen*
            `(:new ,bindings ,env) eval)
      'id 'gid 'eval)


(defun translate-send-progn-or-do (exps c env id gid eval)
  (if (every #'functional-p exps)
    (let* ((translated (mapcar #'translate-embedded exps))
           (ref-vars (apply #'concatenate 'list (mapcar #'second translated))))
      (if (null ref-vars)
        `(send-message ,c 
           (progn 
             ,@(mapcar #'(lambda (exp)
                           (funcall (first exp) nil))
                       translated))
           nil ,eval)
        (let* ((subst (mapcar #'(lambda (var) (cons var (gensym)))
                              ref-vars))
               (pattern (if (rest ref-vars)
                          (cons 'list (mapcar #'cdr subst))
                          (cdr (first subst))))
               (tag (if (rest ref-vars) ':do-evlis ':do))
               (exp (if (rest ref-vars)
                      (cons 'list (mapcar #'(lambda (var)
					      (list 'quote
						    (list :variable var))
					      #+COMMENT
					      `'(:variable ,var))
                                          ref-vars))
		      (list 'quote (list :variable (first ref-vars)))
		      #+COMMENT
                      `'(:variable ,(first ref-vars)))))
          `(send-message eval
             (list ,tag ,exp ,env ,id ,gid ,eval)
	    #+COMMENT
             `(,,tag ,,exp ,,env ,,id ,,gid ,,eval)
             (cont ,pattern
               (send-message ,c
                 (progn ,@(mapcar #'(lambda (exp)
                                      (funcall (first exp) subst))
                                  translated))
                 nil eval))
             eval))))
    `(send-message eval
       (list :do-progn (list ,@(mapcar #'translate exps))
             ,env ,id ,gid ,eval)
       ,c ,eval)))
                                                
(defun translate-user-script (exp)
  `(list :self-evaluate
    #'(lambda () ,(translate-script exp))
    #+COMMENt ,(translate-script exp)))

#+COMMENT
;; continuation as object
(defun translate-cont (pattern body)
  (translate-object 
   nil '(:const nil) '(:const nil) nil
   (list (list :script pattern nil nil body))))
                     
;; continuation as a light weight object
(defun translate-cont (pattern body)
		(let ((lex-env (pickup-pattern-vars pattern)))
		  `(list :compiled
		    #'(lambda (C Env Id Gid Eval &aux (Me Id))
			(send-message C
			  (cont ,pattern
				,@(mapcar #'(lambda (exp)
					      (translate-NRM exp lex-env 'Env))
					  body))
			  nil Eval)))))
#|
;; Following definition causes an error when a [cont ...] expression
;; is generated under no circumstancing expressions.
;; For example, an expression [cont _ t] from toplevel causes an error
;; becasue it produces a code:
;; 	[:self-evaluate
;; 	 (let ((Me Id))
;; 	   (cont _ t))]
;; but variable Id is unbound.
(defun translate-cont (pattern body)
  (let ((lex-env (pickup-pattern-vars pattern)))
    `(list :self-evaluate
      (let ((Me Id))			; Sigh...
	(cont ,pattern
	      ,@(mapcar #'(lambda (exp)
			    (translate-NRM exp lex-env 'Env))
			body))))))
|#

;; Bracket* is function same as list*.
(defun bracket* (&rest arguments) (apply #'list* arguments))

;; time special form
(defun translate-time (body)
  `(list :compiled
         #'(lambda (C Env Id Gid Eval)
             (send-message (create-timer Gid)
               (list C ,(translate body)
                     Env Id Gid Eval)
               nil Eval))))

;; do special form
;; this translation is far to be optimal.
(defun translate-do (var-decls test-form body)
  (let ((do-variables (mapcar #'do-var-name var-decls))
        (do-var-temps (mapcar #'(lambda (dummy)
                                  (declare (ignore dummy))
                                  (gensym))
                              var-decls))
        (do-init-forms (mapcar #'translate (mapcar #'do-var-init var-decls)))
        (update-forms (mapcar #'translate (mapcar #'do-var-step var-decls)))
        (test-form (translate (do-test-test test-form)))
        (result-forms (mapcar #'translate (do-test-results test-form)))
        (body-forms (mapcar #'translate body)))
    `(list :compiled
      #'(lambda (C Env Id Gid Eval)
          (let (expanded-env env-expander branch compute-update assign-update)
            (setq env-expander
                  (cont (list ,@do-var-temps)
                    (setq expanded-env
                          (extend-env (pairlis ',do-variables
                                               (list ,@do-var-temps))
                                      Env))
                    (send-message Eval
                      (list :do ,test-form expanded-env Id Gid Eval)
		      #+COMMENT
                      `(:do ,,test-form ,expanded-env ,Id ,Gid ,Eval)
                      branch Eval)))
            (setq branch
                  (cont test-result
                    (if test-result
                      (send-message Eval
                        (list :do-progn (list ,@result-forms)
			      expanded-env Id Gid Eval)
			#+COMMENT
                        `(:do-progn ,(list ,@result-forms) ,expanded-env ,Id ,Gid ,Eval)
                        C Eval)
                      (send-message Eval
                        (list :do-progn (list ,@body-forms)
			      expanded-env Id Gid Eval)
			#+COMMENT
                        `(:do-progn ,(list ,@body-forms) ,expanded-env ,Id ,Gid ,Eval)
                        compute-update Eval))))
            (setq compute-update
                  (cont dummy
                    (send-message Eval
                      (list :do-evlis (list ,@update-forms)
			expanded-env Id Gid Eval)
		      #+COMMENT
                      `(:do-evlis ,(list ,@update-forms) ,expanded-env ,Id ,Gid ,Eval)
                      assign-update Eval)))
            (setq assign-update
                  (cont (list ,@do-var-temps)
		    ,@(mapcar #'(lambda (temp-var real-name)
                                 `(send-message expanded-env
                                    (list :set ',real-name ,temp-var)
                                    nil Eval))
                             do-var-temps do-variables)
                    (send-message Eval
                      (list :do ,test-form expanded-env Id Gid Eval)
		      #+COMMENT
                      `(:do ,,test-form ,expanded-env ,Id ,Gid ,Eval)
                      branch Eval)))
          (send-message Eval
            (list :do-evlis (list ,@do-init-forms) Env Id Gid Eval)
	    #+COMMENT
            `(:do-evlis ,(list ,@do-init-forms) ,Env ,Id ,Gid ,Eval)
            env-expander Eval))))))

;; inc special form
;; An execution of (inc <args>...) causes a call:
;; 	(inc-main p C Env Id Gid Eval)
;; where p is list of evaluated <args>, and C, Env, Id, Gid, and Eval
;; are the values of the run-time context of the execution of inc.
(defun translate-inc (args)
  `(list :compiled
    #'(lambda (C Env Id Gid Eval)
	(send-message Eval
	  (list :do-evlis (list ,@(mapcar #'translate args))
	    Env Id Gid Eval)
	  (cont parameters
		(inc-main parameters C Env Id Gid Eval))
	  Eval))))

;; reify-args special form
;; An execution of (reify-args <Keyword> ...) causes a message to a
;; metaobject or an evaluator. The message sent should be have a form
;; like following:
;;	[<Keyword> <Body> <Env> <Id> <Gid> <Eval>]
;; where <Body> be a compiled object of the body part of the form. 
;; The reply destination of this message should be the continuation of
;; the form.
(defun translate-reify-args (dest keyword body)
  (with-eval (list (translate keyword))
    '(C Env Id Gid Eval)
    #'(lambda (value)
        `((send-message ,(ecase dest
                          (:eval 'Eval)
                          (:meta 'Id))
           (list ,(first value)
                 (list ,@(mapcar #'translate body))
                 Env Id Gid Eval)
           C Eval)))))