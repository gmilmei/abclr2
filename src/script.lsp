;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; script creation, looking up, execution
;;; Nov. 9, 1991 by MASUHARA, H.
;;; Jul.23, 1992 --- rewritten entirely ---
;;;             o A script is now able to be treated as a LW-object as well
;;;		  as Lisp's structure.
;;;		o New interfaces such as look-up script pattern is added.
;;; Dec.14, 1994: a new where-exp entry for a script is added.

(in-package "ABCLR2-SYSTEM")

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Ineterface Description:

1. As an object

[object aScript
 (state ...)
 (script
  (=> [:match message reply sender]
      !<bindings if matches, nil if not>)
  (=> [:exec Env Id Gid Eval] @ C
      <execute the body of script>)
  (=> :pattern
      !<list of pattern, reply-des, and sender>)
  (=> :body
      !<body of script>)
  (=> :where
      !<where expression>))]

2. As a Lisp's structure

(make-script pattern reply from body)
  A script type data object is created and returned.

(script-match-p message reply sender script)
  Matching of a triplet of <message,reply,sender> and the given script
  is tested.  If matches a list of bindings and script itself is returned.

(call-script-body script C Env Id Gid Eval)
  A body of script is executed under an environment Env.

3. Higher-order functions

(find-script message reply sender scriptset)
  Matching script for message and reply is searched  in scriptset,
  and a list which contains a list of variable names, 
  a list of variable values and script body is returned.
  If there are no match made, nil is returned.

(search-script message reply sender scriptset)
  The scriptset is searched and when there is a script which matchs
  the given message and reply, the script itself is returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(defstruct script-local
  (message-pattern nil :type t)
  (pattern-matching-fun #'(lambda (dummy)
			    (declare (ignore dummy))
			    nil)
			:type (function (t) t))
  (reply-variable nil :type symbol)
  (from-field nil :type symbol)
  (body nil :type list)
  (where-exp nil :type t))

;;
;; internal functions --- for data type script-local.
;;

(defun script-match-p-internal (message reply sender script-local)
  (declare (type script-local script-local))
  (let ((binding 
         ;; (Compiled) pattern matcher is stored
         ;; at message-pattern slot.
         (funcall (script-local-pattern-matching-fun script-local)
                  message)))
    (if (null binding) 
      nil	; fail
      (progn 
        ;; message matched without binding
        (if (eq binding t) (setq binding nil))
        (when (script-local-reply-variable script-local)
          (push (cons (script-local-reply-variable script-local) reply)
                binding))
        (when (script-local-from-field script-local)
          (push (cons (script-local-from-field script-local) sender)
                binding))
        (or binding t)))))

(defun call-script-body-internal (script-local Cont Env Id Gid Eval)
  (declare (type script-local script-local))
  (execute-and-catch-error 'compiled-script-execution
   (send-message Eval
		 (list :do-progn
		       (script-local-body script-local) Env Id Gid Eval)
		 Cont Eval)))

;;
;; message receiver for internal script strcutre
;;

(defun internal-script-port (script-object M R S)
  (let ((script-internal (object-data-local script-object)))
    ;;
    (match M
      (is (list :match message reply sender)
	  (send-message R 
			(script-match-p message reply sender 
					script-object)
			nil script-object))
      (is (list :exec Env Id Gid Eval)
	  (call-script-body-internal script-internal
				     R Env Id Gid Eval))
      (is :pattern
	  (send-message R (list (script-local-message-pattern script-internal)
				(script-local-reply-variable script-internal)
				(script-local-from-field script-internal))
			nil script-object))
      (is :body
	  (send-message R (script-local-body script-internal)
			nil script-object))
      (is :where-exp
	  (send-message R (script-local-where-exp script-internal)
			nil script-object))
      (otherwise
       (error "Script object ~S cannot handle a message ~S~%~
               (reply=~S, sender=~S)"
	      script-object M R S)))))
        
(defun create-script-object (meta-group script-internal)
  (make-object-data :port #'internal-script-port
                    :meta nil
                    :den nil
                    :gid meta-group
                    :local script-internal
                    :type 'Cont
                    :status 'no-schedule
                    :status-lock nil
                    :name 'script))
;;
;; interface functions --- script is script-local or ordinary object
;;

(defun internal-script-structure-p (object)
  (and (object-data-p object)
       (script-local-p (object-data-local object))))

(defun script-match-p (message reply sender script-object)
  (if (internal-script-structure-p script-object)
      (let ((binding (script-match-p-internal
		      message reply sender
		      (object-data-local script-object))))
	(and binding (list (if (eq binding t)
			       nil binding)
			   (list :script script-object))))
      (send-present-message script-object
			    (list :match message reply sender)
			    :Anonymous)))

(defun call-script-body (script-object C Env Id Gid Eval)
  (cond ((internal-script-structure-p script-object)
	 (call-script-body-internal (object-data-local script-object)
				    C Env Id Gid Eval))
	((object-data-p script-object)
	 (send-message script-object
		       (list :exec Env Id Gid Eval)
		       C Eval))
	((functionp script-object)
	 (funcall script-object C Env Id Gid Eval))))

(defmacro make-script (pattern reply from body &optional (gid nil) where-exp)
  `(create-script-object 
    ,gid
    (make-script-local
     :message-pattern ',pattern
     :pattern-matching-fun (make-script-binding-function ,pattern)
     :reply-variable ,reply
     :from-field ,from
     :body ,body
     :where-exp ,where-exp)))

;;
;; Higher level functions --- either usual object or internal script
;;

(defun find-script (message reply sender scriptset)
  (cond ((null scriptset)
         nil)
        (t
         (or (script-match-p message reply sender (first scriptset))
             (find-script message reply sender (rest scriptset))))))

(defun search-script (message reply sender scriptset)
  (find-if #'(lambda (script)
	       (script-match-p message reply sender script))
	   scriptset))

(defun remove-script (message reply sender scriptset)
  (remove-if #'(lambda (script)
		 (script-match-p message reply sender script))
	     scriptset
	     :count 1))
