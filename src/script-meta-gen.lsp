;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; "Hand compiled" script for primary metaobject generator
;;; Nov. 17, 1991 by MASUHARA, H.
;;; Jul. 25, 1992 --- script with from field.

(in-package "ABCLR2-SYSTEM")

(defvar *primary-meta-gen-scriptset*)

;; As the default meta-gen has no state variables,
;; this definition simply delegates to the meta-gen in
;; the meta-level.

;;[object meta-gen
;; (script
;;  (=> Any @ R
;;      [[Group <== :meta-gen] <= Any @ R]))]

(setf *primary-meta-gen-scriptset*
      (list
       (make-script
	Any
	'R
	nil
	`((:compiled
	   ,#'(lambda (C Env Id Gid Eval)
		(send-message Eval
		  `(:do (:variable Group) ,Env ,Id ,Gid ,Eval)
		  (cont Group
		    (send-message Eval
		      `(:do (:send-now ,Group :meta-gen)
			,Env ,Id ,Gid ,Eval)
		      (cont meta-gen
			(send-message Eval
			  `(:do-evlis ((:variable Any) (:variable R))
			    ,Env ,Id ,Gid ,Eval)
			  (cont (list Any R)
			    (send-message Eval
			      `(:do (:send-past ,meta-gen ,Any ,R)
				,Env ,Id ,Gid ,Eval)
			      C Eval))
			  Eval))
		      Eval))
		  Eval)))))))
