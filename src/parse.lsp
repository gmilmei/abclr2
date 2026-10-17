;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; script parser
;;; Nov. 23, 1991 by MASUHARA, H.
;;; Apr. 19, 1992 --- special form time is added.
;;; June 15, 1992 --- special form "do" is added.
;;; July 17, 1992 --- future type message is added.
;;; July 25, 1992 --- script with from field.
;;; July 27, 1992 --- script with WHERE field.
;;; July 29, 1992 --- statespec without initial value is allowed.
;;; July 29, 1992 --- contexnt sensitive inc.
;;; July 31, 1992 --- bug of DO in defun is fixed.
;;; Aug.  1, 1992 --- DECLARE form is now ignored.
;;; Nov.  8, 1992 --- pass through DEFCLASS, DEFGENERIC, DEFMETHOD.
;;; Dec. 11, 1992 --- [GROUP-OF e] is now available.
;;; Jan.  8, 1993 --- in [group ...], meta-gen & evaluator become optional.
;;; Jan. 23, 1993 --- EVAL-WHEN forms can be parsed now, but their
;;; 			bodies are executed at any situation.
;;; Feb. 25, 1993 --- pass through DEFSTRUCT.
;;; Jun. 13, 1993 --- special form reify-args is added.
;;; Jul. 28, 1994 --- special-operator-p for some CLs
;;; Jan. 12, 1995 --- a bug in DO form is fixed
;;; Jan. 19, 1995 --- parsing rule for object is slightly fixed

(in-package "ABCLR2-SYSTEM")

;; A bracket expression "[a b c]" is internally represented by
;; "(bracket a b c)".
;; Dotted notation is represented by *-appended bracket.
;; (e.g. "[a b .c]" is "(bracket* a b c)")
;; Exclamation expression is "!x" is "(exclam x)".

(defmacro check (test message exp)
  `(unless ,test
     (warn (format nil "~A~%while parsing ~S~%"
                   ,message ,exp))))

;; assignment
;; (bracket <name> := <exp> <extra>*)
;; --> (:assign |name| <exp> <extra>)
(defun parse-assign (oexp reply-dest &aux (exp (rest oexp)) target value extra)
  (check (symbolp (first exp)) "assignment target should be a symobl" exp)
  (setq target (pop exp))
  (check (= 4 (length oexp)) "wrong number of elements for assinment" oexp)
  (pop exp)
  (setq value (parse (pop exp) reply-dest))
  (if exp 
    (setq extra (mapcar #'(lambda (exp)
                            (parse exp reply-dest)) 
                        exp)))
  `(:assign ,target ,value ,extra))
(defun assign-dest (pexp) (second pexp))
(defun assign-exp (pexp) (third pexp))
(defun assign-extra (pexp) (fourth pexp))
(defun assign-p (pexp) (eq (first pexp) :assign))

;; present-type message
;; (bracket <exp> <== <exp> <extra>*)
;; --> (:present <exp> <exp>)
(defun parse-present (oexp reply-dest &aux (exp (cdr oexp)) target message )
  (setq target (parse (pop exp) reply-dest))
  (pop exp)
  (setq message (parse (pop exp) reply-dest))
  (check (null exp) "too many arguments for present type message sending"
         oexp)
  `(:present ,target ,message 
             ,(mapcar #'(lambda (exp)
                          (parse exp reply-dest))
                      exp)))
(defun present-target (pexp) (second pexp))
(defun present-message (pexp) (third pexp))
(defun present-extra (pexp) (fourth pexp))

;; past-type message
;; (bracket <exp> <= <exp> {@ <exp>} <extra>*)
;; --> (:past <target> <message> <reply> <extra>)
;; (bracket <exp> <= <exp> $ <exp> <extra>*)
;; --> (:future <target> <message> <repyl> <extra>)
(defun parse-past (oexp reply-dest
                        &aux (exp (cdr oexp)) target message
			(reply '(:const nil)) future)
  (setq target (parse (pop exp) reply-dest))
  (pop exp)				; skip `<='
  (setq message (parse (pop exp) reply-dest))
  (cond ((null exp))
        ((eq (first exp) '@)
         (pop exp)			; skip @
         (setq reply (parse (pop exp) reply-dest)))
	((eq (first exp) '$)
	 (pop exp)			; skip $
	 (setq reply (parse (pop exp) reply-dest)
	       future t)))
  (check (null exp) "too many arguments for past type message sending"
         oexp)
  `(,(if future :future :past) ,target ,message ,reply 
          ,(mapcar #'(lambda (exp) 
                       (parse exp reply-dest))
                   exp)))
(defun past-target (pexp) (second pexp))
(defun past-message (pexp) (third pexp))
(defun past-reply (pexp) (fourth pexp))
(defun past-extra (pexp) (fifth pexp))

;; variable reference
;; <name>
;; --> (:var |name|)
(defun parse-var (exp) `(:var ,exp))
(defun var-name (pexp) (second pexp))

;; constant
;; <form>
;; --> (:const |form|)
(defun parse-const (exp) `(:const ,exp))
(defun const-body (pexp) (second pexp))

;; script
;; (=> <pattern> {@ <reply>} {from <sender>}
;;     {(temporary <assignment>*)} <exp>*)
;; --> (:script |pattern| |reply| <temporary-assign-list> <exp-list>)
(defun parse-script (exp &aux pattern (reply nil) (sender nil)
			 (constraint nil) (temporary))
  (assert (eq '=> (pop exp)))
  (setq pattern (pop exp))
  (if (eq '@ (first exp))
    (progn (pop exp) (setq reply (pop exp)))
    (setq reply (gensym)))
  (when (eq 'from (first exp))
    (progn (pop exp)
	   (setq sender (pop exp))
	   (unless (symbolp sender)
	     (error "from field should be an symbol but ~S is supplied."
		    exp))))
  (when (eq 'where (first exp))
    (pop exp)
    (setq constraint (parse (pop exp) reply)))
  (if (and (consp (first exp)) (eq 'temporary (first (first exp))))
      (setq temporary
	    (mapcar #'(lambda (exp)
			(if (symbolp exp)
			    (parse (list 'bracket exp ':= nil) reply)
			    (parse exp reply)))
                    (rest (pop exp)))))
  `(:script ,pattern ,reply ,temporary 
            ,(mapcar #'(lambda (exp)
                         (parse exp reply))
                     exp) ,sender ,constraint))
(defun script-pattern (pexp) (second pexp))
(defun script-reply (pexp) (third pexp))
(defun script-temporary (pexp) (fourth pexp))
(defun script-exps (pexp) (fifth pexp))
(defun script-from (pexp) (sixth pexp))
(defun script-where (pexp) (seventh pexp))

;; object-definition
;; (bracket object {<name>}
;;   {(meta-gen <exp>|non-reifying-meta)}
;;   {(group <exp>)}
;;   {(state <assign>*)}
;;   (script <script>*)
;;   {(<tag> <exp>)*})
;; --> (:object |name| <meta-gen> <state-list> <script-list> <extra>), or
;; --> (:NRM-object |name|
;;             :non-reifying-meta <state-list> <script-list> <extra>)
;;     where <extra> is ((<keyword> <exp>)*).
(defun parse-object (original-exp reply-dest &aux
                                  (exp original-exp) (name nil)
                                  (meta-gen-spec '(:const nil))
                                  (group-spec '(:const nil))
                                  (state nil) (script :no-script)
				  (object-type ':object) (extra nil))
  ;; skip "bracket" and "object"
  (pop exp) (pop exp)
  ;; read name if exists
  (if (symbolp (first exp)) (setq name (pop exp)))
  (assert (consp exp))
  (dolist (spec exp)
    (assert (consp spec))
    (case (first spec)
      (meta-gen
       (let ((meta-gen-exp (second spec)))
	 (if (eq meta-gen-exp 'non-reifying-meta)
	     (setq object-type ':NRM-object))
	 (setq meta-gen-spec (parse meta-gen-exp reply-dest))))
      (group
       (setq group-spec (parse (second spec) reply-dest)))
      (state
       (setq state (mapcar #'(lambda (exp)
			       (if (symbolp exp)
				   (parse `(bracket ,exp := nil) reply-dest)
				   (parse exp reply-dest)))
			   (rest spec))))
      (script
       (setq script (mapcar #'parse-script (rest spec))))
      (t
       (assert (symbolp (first spec)))
       (warn "~S is not standard to object definition, it is treated as ~
		special keywords" (first spec))
       (push (list (intern (symbol-name (first spec)) 'keyword)
		   (parse (second spec) reply-dest))
	     extra))))

  (list object-type name meta-gen-spec group-spec state
	script (reverse extra)))
(defun object-name (pexp) (second pexp))
(defun object-meta-gen (pexp) (third pexp))
(defun object-group (pexp) (fourth pexp))
(defun object-state-list (pexp) (fifth pexp))
(defun object-script-list (pexp) (sixth pexp))
(defun object-extra (pexp) (seventh pexp))

;; group definition
;; (bracket group {<name>}
;;  (meta-gen <exp>)
;;  (evaluator <exp>)
;;  {(resources {[<name> := <exp>]}*)}
;;  {(script <script>*)}
;;  {(initialize <exp>*)}
;;  {(initialize-meta <exp>*)})
;; --> (:group |name| <meta-gen> <evaluator> <resouces> <scripts>
;;                    <initialize> <initialize-meta>)
(defun parse-group (exp &aux (p exp) name
			(meta-gen '(create-primary-meta-gen group))
			(evaluator '(create-primary-eval group))
			resources script initialize initialize-meta)
  (pop p) (pop p) ;; discard "bracket" and "group"
  (if (symbolp (first p)) (setq name (pop p)))
  (dolist (decl p
	   (progn   (check (and meta-gen evaluator)
			   "Group definition without meta-gen or evaluator."
			   exp)
		    (list :group name (parse meta-gen)
			  (parse evaluator)
			  resources script initialize initialize-meta)))
    (unless (consp decl)
      ;; parsing expression may be rail
      (return nil))
    (case (first decl)
      (meta-gen
       ;; (check (null meta-gen) "More than one declaration for meta-gen" exp)
       (setq meta-gen (second decl)))
      (evaluator
       ;;(check (null evaluator)
       ;;  "More than one declaration for evaluator" exp)
       (setq evaluator (second decl)))
      (resources
       (check (null resources) "More than one declaration for resources" exp)
       (setq resources (mapcar #'parse (rest decl))))
      (script
       (check (null script) "More than one declaration for script" exp)
       (setq script (mapcar #'parse-script (rest decl))))
      (initialize
       (check (null initialize) "More than one declaration for initialize" exp)
       (setq initialize (mapcar #'parse (rest decl))))
      (initialize-meta
       (check (null initialize-meta)
	      "More than one declaration for initialize-meta" exp)
       (setq initialize-meta (mapcar #'parse (rest decl))))
      (t (return nil)))))
(defun group-name (pexp) (second pexp))
(defun group-meta-gen (pexp) (third pexp))
(defun group-evaluator (pexp) (fourth pexp))
(defun group-resources (pexp) (fifth pexp))
(defun group-scripts (pexp) (sixth pexp))
(defun group-initialize (pexp) (seventh pexp))
(defun group-initialize-meta (pexp) (eighth pexp))

;; meta
;; (bracket meta <exp>)
;; --> (:meta <exp>)
(defun parse-meta (exp reply-dest)
  (check (= 3 (length exp)) "Wrong number of arguments for meta" exp)
  (parse (cons 'meta-of (cddr exp)) reply-dest))

;; den
;; (bracket den <exp>)
;; --> (:den <exp>)
(defun parse-den (exp reply-dest)
  (check (= 3 (length exp)) "Wrong number of arguments for den" exp)
  (parse (cons 'den-of (cddr exp)) reply-dest))
(defun den-exp (pexp) (second pexp))

;; cont
;; (bracke cont <pattern> <exp>*)
;; --> (:cont <pattern> <exps>)
(defun parse-cont (exp reply-dest)
  (list ':cont (third exp)
        (mapcar #'(lambda (exp)
                    (parse exp reply-dest))
                (cdddr exp))))
(defun cont-pattern (pexp) (second pexp))
(defun cont-body (pexp) (third pexp))

;; group-of
;; (bracket group-of <exp>)
;; --> (group-of <exp>)
(defun parse-group-of (exp reply-dest)
  (parse (cons 'group-of (cddr exp)) reply-dest))

;; rail
;; [<exp>*]
;; --> (:rail <exp>*)
(defun parse-rail (exp reply-dest)
  `(:rail ,(mapcar #'(lambda (exp)
                       (parse exp reply-dest))
                   (rest exp))))
(defun rail-body (pexp) (second pexp))

;; bracket expressions
(defun parse-bracket (exp reply-dest &aux
			  (e1 (second exp)) (e2 (third exp))
			  (len (- (length exp) 1)))
  (assert (eq 'bracket (first exp)))
  (cond ((eq '<= e2) (parse-past exp reply-dest))
        ((eq '<== e2) (parse-present exp reply-dest))
        ((eq ':= e2) (parse-assign exp reply-dest))
        ((and (eq 'object e1) 
	      ;; Thu Jan 19 14:00:23 JST 1995
	      ;; We regard any bracket expressions begin with 'object as
	      ;; object definitions.
	      #+COMMENT(member len '(2 3 4 5 6) :test #'=))
	 (parse-object exp reply-dest))
        ((and (eq 'meta e1) (= len 2))
	 (parse-meta exp reply-dest))
        ((and (eq 'den e1) (= len 2))
	 (parse-den exp reply-dest))
	((and (eq 'script e1) (= len 2))
	 (parse-script (third exp)))	; as a first class object
        ((and (eq 'group e1) (parse-group exp)))
        ((and (eq 'cont e1) (< 1 len))
         (parse-cont exp reply-dest))
	((and (eq 'group-of e1) (= len 2))
	 (parse-group-of exp reply-dest))
        (t (parse-rail exp reply-dest))))

;; exclamation
;; (exclam <exp>) --> (bracket <reply-dest> <= <exp>)
(defun parse-reply (exp reply-dest)
  (parse `(bracket ,reply-dest <= ,(second exp))
         reply-dest))
  
;; match
;; (match <exp> (is <pattern> <exp>*)* {(otherwise <exp>*)})
;; --> (:match <exp> <isclauses> <otherwise>)
(defun parse-match (exp reply-dest &aux source-exp isclauses otherwise)
  (pop exp) ;; drop 'match
  (setq source-exp (parse (pop exp) reply-dest))

  (when (eq 'otherwise (caar (last exp)))
    (setq otherwise (mapcar #'(lambda (exp)
                                (parse exp reply-dest))
                            (cdar (last exp))))
    (setq exp (butlast exp)))
  
  (setq isclauses
        (mapcar #'(lambda (exp)
              (parse-is-clause exp reply-dest))
          exp))
  
  `(:match ,source-exp ,isclauses ,otherwise))
(defun match-exp (pexp) (second pexp))
(defun match-isclauses (pexp) (third pexp))
(defun match-otherwise (pexp) (fourth pexp))

;; is-clause
;; (is <pattern> <exp>*)*
;;  --> (:is |pattern| <exps>)
(defun parse-is-clause (is-clause reply-dest)
  (check (eq 'is (first is-clause)) "Illegal syntax for is clause" is-clause)
  ;;
  `(:is ,(second is-clause) ,(mapcar #'(lambda (exp)
                                         (parse exp reply-dest))
                                     (cddr is-clause))))
(defun is-pattern (pexp) (second pexp))
(defun is-exps (pexp) (third pexp))

;; function application
;; (<name> <exp>*)
;; --> (:apply |funcname| <exp>*)
(defun parse-apply (exp reply-dest) 
  `(:apply ,(first exp) ,(mapcar #'(lambda (exp)
                                     (parse exp reply-dest))
                                 (rest exp))))
(defun apply-func (pexp) (second pexp))
(defun apply-args (pexp) (third pexp))

;; composed function application
;; (:composed <producing-func> <variable-list>)
(defun composed-varlist (pexp) (third pexp))
(defun composed-producer (pexp) (second pexp))

;; special forms
(defun parse-special (exp reply-dest &aux (len (length exp)))
  (flet ((parse-sub (exp)
           (parse exp reply-dest)))
    (case (first exp)
      #+:SBCL
      (the
       ;; SBCL macro expansions can wrap expressions in a vacuous THE T.
       ;; Parse the operand normally, preserving its reply destination.
       ;; Other type assertions and malformed forms remain unsupported.
       (when (and (= len 3) (eq (second exp) t))
         (parse-sub (third exp))))
      (if (check (member len '(3 4) :test #'=)
                 "Wrong number arguments for if"
                 exp)
        (list* :if (mapcar #'parse-sub (rest exp))))
      (progn 
        (list* :progn (mapcar #'parse-sub (rest exp))))
      (let
        (list :let (parse-let-vars (second exp) reply-dest)
              (mapcar #'parse-sub (rest (rest exp)))))
      (let*
        (parse-let* exp reply-dest))
      (setq
       (check (= len 3) "Only single assignment is allowed for setq."
              exp)
       (list :assign (second exp) (parse-sub (third exp)) nil))
      ((defun function defclass defgeneric defmethod defstruct)
       (list :func exp))
      (r2user::time
       (check (= len 2) "Wrong number of arguments for time." exp)
       (list :time (parse-sub (second exp))))
      (defmacro
        (eval exp) ;; its effect should appear immediately
        ;; Although expressions containing macros are expanded before
        ;; compilation, defmacro expression will be evaluated
        ;; at the load-time for the succeeding interactive session.
        (list :func exp))
      (do                               ; not a macro
        (check (<= 3 len )
                   "Too few arguments for do" exp)
        (list :do 
                (parse-do-vars (second exp) reply-dest)
                (parse-do-test (third exp) reply-dest)
                (mapcar #'parse-sub (nthcdr 3 exp))))
      (inc
       (list :inc (mapcar #'parse-sub (rest exp))))
      (declare
       (warn "DECLARE form ~S is ignored." exp)
       (parse nil))
      (eval-when
        (warn "The body of ~S is always executed." exp)
        (parse (cons 'progn (nthcdr 2 exp))))
      (t nil))))

(defun if-test (pexp) (second pexp))
(defun if-then (pexp) (third pexp))
(defun if-else (pexp) (fourth pexp))
(defun func-body (pexp) (second pexp))
(defun time-body (pexp) (second pexp))
(defun do-vars (pexp) (second pexp))
(defun do-test (pexp) (third pexp))
(defun do-body (pexp) (fourth pexp))
(defun inc-args (pexp) (second pexp))

(defun parse-let-vars (assigns reply-dest)
  (mapcar #'(lambda (assign)
              (if (consp assign)
                (parse (list 'bracket (first assign) ':= (second assign))
                       reply-dest)
                (parse (list 'bracket assign ':= nil)
                       reply-dest)))
          assigns))
(defun let-assigns (pexp) (second pexp))
(defun let-body (pexp) (third pexp))

(defun let*-to-let (binding exps reply-dest)
  (if (or (null binding) (null (rest binding)))
      `(let ,binding ,@exps)
      `(let (,(first binding))
	,(let*-to-let (rest binding) exps reply-dest))))

(defun parse-let* (exp reply-dest)
  (parse (let*-to-let (second exp) (rest (rest exp)) reply-dest)))

(defun parse-do-vars (exp reply-dest)
  ;; (do ((V1 INIT1 UPDATE1) (V2 INIT2 UPDATE2) ...) ...)
  (mapcar
   #'(lambda (var-form)
       (let ((name (first var-form))
             (init (second var-form))
             (step (third var-form)))
         (list (first var-form) 
               (if init 
                 (parse (second var-form) reply-dest)
                 (parse 'nil))
               (if step
                 (parse (third var-form) reply-dest)
                 (parse name)))))
   exp))

(defun parse-do-test (exp reply-dest)
  ;; (do (...) (END-TEST . RESULT) ...)
  (list (parse (first exp) reply-dest)
        (mapcar #'(lambda (subexp) (parse subexp reply-dest))
                (rest exp))))	; do not duplicate end-test...

(defun do-var-name (parsed-var-form) (first parsed-var-form))
(defun do-var-init (parsed-var-form) (second parsed-var-form))
(defun do-var-step (parsed-var-form) (third parsed-var-form))
(defun do-test-test (parsed-test-form) (first parsed-test-form))
(defun do-test-results (parsed-test-form) (second parsed-test-form))

;; reify-args & reify-args-to-meta
;; (reify-args <Keyword> . <Body>) 
;; -> (:reify-args :eval <Keyword>* <Body>*)
;; (reify-args-to-meta <Keyword> . <Body>) 
;; -> (:reify-args :meta <Keyword>* <Body>*)
(defun reify-args-dest    (parsed-form) (second parsed-form))
(defun reify-args-keyword (parsed-form) (third  parsed-form))
(defun reify-args-body    (parsed-form) (fourth parsed-form))

(defun parse-reify-args (exp &optional reply-dest)
  (check (> (length exp) 2)
         "Insufficient number of arguments for reify-args."
         exp)
  (list :reify-args
        ;; destination
        (ecase (first exp)
          (reify-args         :eval)
          (reify-args-to-meta :meta))
        ;; keyword
        (progn 
          (unless (keywordp (second exp))
            (warn "Keyword for a reify-args form is dynamic. ~
                   Keyword form is ~S, in form ~S."
                  (second exp) exp))
          (parse (second exp) reply-dest))
        ;; body
        (mapcar #'(lambda (subexp) (parse subexp reply-dest))
                (cdr (cdr exp)))))

;; pexp main
(defun parse (exp &optional reply-dest)
  (cond ((constantp exp) 
         #+COMMENT (parse-const (eval exp))
         (parse-const exp))
        ((symbolp exp) (parse-var exp))
        ((consp exp)
         (cond ((eq 'bracket (first exp))
                (parse-bracket exp reply-dest))
               ((eq 'exclam (first exp))
                (parse-reply exp reply-dest))
               ((eq 'match (first exp))
                (parse-match exp reply-dest))
               ((member (first exp) '(reify-args reify-args-to-meta)
                        :test #'eq)
                (parse-reify-args exp reply-dest))
               (#+COMMENT(special-form-p (first exp))
                (parse-special exp reply-dest))
               ((macro-function (first exp))
                (parse (macroexpand-1 exp) reply-dest))
	       ((#+(or excl :CMU) special-operator-p
		 #-(or excl :CMU) special-form-p
		   (first exp))
		(error "special form ~S is not supported." exp))
               (t (parse-apply exp reply-dest))))
        (t (error "wrong type expression ~S" exp))))
