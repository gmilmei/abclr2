;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; "Hand compiled" script for primary group mananger
;;; Nov. 18, 1991 by MASUHARA, H.
;;; Jul. 18, 1992 --- replacing group kernel objects are added.
;;; Jul. 22, 1992 --- minor bug fix
;;; Jul. 25, 1992 --- script with from field.
;;; Feb. 24, 1993 --- changes around self-eval type expressions
;;; Mar. 17. 1994 --- a bug on replacement of evaluator is fixed.
;;; Dec. 12, 1994 --- now accepts extra parameters for object creation.

(in-package "ABCLR2-SYSTEM")

(defvar *primary-GMgr-scriptSet*)

;;[object GMgr
;; (state [evaluator := ...]
;;	[meta-gen := ...])
;; (script
;;  (=> :initialize )			; do nothing
;;  (=> :meta-gen !meta-gen)
;;  (=> :evaluator !evaluator)
;;  (=> [:new StateSpec Env Script] @ C
;;      [meta-gen <= [:new StateSpec Env Script evaluator Me] @ C])
;;  (=> [:new-group StateSpec Env Script] @ C
;;      [Group <= [:new StateSpec Env Script] @ C])
;;  (=> [:replace-meta-gen new-meta-gen]
;;      [meta-gen := new-meta-gen])
;;  (=> [:replace-evaluator new-eval]
;;      [evaluator := new-eval]))]

(setf *primary-GMgr-scriptSet*
      (list
       (make-script
	:initialize
	nil
	nil
	`((:self-evaluate ,#'(lambda () nil))))
       ;;
       (make-script
	:meta-gen
	'R
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
		(send-message Eval
		  `(:do-evlis ((:variable R) (:variable meta-gen))
		    ,Env ,Id ,Gid ,Eval)
		  (cont (list R meta-gen)
		    (send-message Eval
		      `(:do (:send-past ,R ,meta-gen nil)
			,Env ,Id ,Gid ,Eval)
		      C Eval))
		  Eval)))))
       ;;
       (make-script
	:evaluator
	'R
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
		(send-message Eval
		  `(:do-evlis ((:variable R) (:variable evaluator))
		    ,Env ,Id ,Gid ,Eval)
		  (cont (list R evaluator)
		    (send-message Eval
		      `(:do (:send-past ,R ,evaluator nil)
			,Env ,Id ,Gid ,Eval)
		      C Eval))
		  Eval)))))
       ;;
       ;; Mon Dec 12 15:12:07 JST 1994
       ;; modified so that it will accept extra parameters
       (make-script
	(list* :new StateSpec Env Script Extra)
	'C
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
                ;; (format t "Creating a new object...~%")
		(send-message Eval
		  `(:do-evlis ((:variable meta-gen)
			       (:variable StateSpec)
			       (:variable Env)
			       (:variable Script)
			       (:variable evaluator)
			       (:variable Me)
			       (:variable C)
			       (:variable Extra))
		    ,Env ,Id ,Gid ,Eval)
		  (cont (list meta-gen StateSpec Env* Script evaluator Me C*
			      Extra)
		    (send-message Eval
		      `(:do (:send-past ,meta-gen
			     (:new ,StateSpec ,Env* ,Script ,evaluator ,Me
				    ,@Extra)
			     ,C*) ,Env ,Id ,Gid ,Eval)
		      C Eval))
		  Eval)))))
#+COMMENT
       (make-script
	(list :new StateSpec Env Script)
	'C
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
                ;; (format t "Creating a new object...~%")
		(send-message Eval
		  `(:do-evlis ((:variable meta-gen)
			       (:variable StateSpec)
			       (:variable Env)
			       (:variable Script)
			       (:variable evaluator)
			       (:variable Me)
			       (:variable C))
		    ,Env ,Id ,Gid ,Eval)
		  (cont (list meta-gen StateSpec Env* Script evaluator Me C*)
		    (send-message Eval
		      `(:do (:send-past ,meta-gen
			     (:new ,StateSpec ,Env* ,Script ,evaluator ,Me)
			     ,C*) ,Env ,Id ,Gid ,Eval)
		      C Eval))
		  Eval)))))
       ;;
       (make-script
	(list :new-group StateSpec Env Script)
	'C
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
		(send-message Eval
		  `(:do-evlis ((:variable Group)
			       (:variable StateSpec)
			       (:variable Env)
			       (:variable Script)
			       (:variable C))
		    ,Env ,Id ,Gid ,Eval)
		  (cont (list Group StateSpec Env* Script C*)
		    (send-message Eval
		      `(:do (:send-past ,Group
			     (:new ,StateSpec ,Env* ,Script)
			     ,C*)
			,Env ,Id ,Gid ,Eval)
		      C Eval))
		  Eval)))))
       ;;
       (make-script
	(list :replace-meta-gen new-meta-gen)
	nil
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
		(send-message Eval
		  `(:do (:variable new-meta-gen) ,Env ,Id ,Gid ,Eval)
		  (cont new-meta-gen
		    (send-message Env
		      `(:set meta-gen ,new-meta-gen)
		      C Eval))
		  Eval)))))
       ;;
       (make-script
	(list :replace-evaluator new-evaluator)
	nil
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
		(send-message Eval
		  `(:do (:variable new-evaluator) ,Env ,Id ,Gid ,Eval)
		  (cont new-evaluator
		    (send-message Env
		      `(:set evaluator ,new-evaluator)
		      C Eval))
		  Eval)))))
))
