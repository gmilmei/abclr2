;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; "Hand compiled" script for primary evaluator
;;; Nov. 17, 1991 by MASUHARA, H.
;;; Jul. 25, 1992 --- script with from field.

(in-package "ABCLR2-SYSTEM")

;; The default script code of the primary evaluator is the one
;; that simply delegates all messages to a evaluator in its
;; meta-level.
;;
;; [object evaluator
;;  (script
;;   (=> Any @ R
;;       [[Group <== :evaluator]
;;        <= Any @ R]))]


(defvar *primary-evaluator-script*)

(setf *primary-evaluator-script*
      (list
       (make-script
        Any				;pattern (unevaled)
        'R				;reply
	nil				;from
        `((:compiled
           ,#'(lambda (C Env Id Gid Eval)
                (send-message Eval
                  `(:do (:variable Group) ,Env ,Id ,Gid ,Eval)
                  (cont GMgr
                    (send-message Eval
                      `(:do-evlis ((:send-now ,GMgr :evaluator)
                                   (:variable Any) (:variable R)
                                   (:variable Me))
                                  ,Env ,Id ,Gid ,Eval)
                      (cont (list delegating-eval Any R Me)
                        #+:ABCLR2-DEBUG
                        (debug-print "Reified eval ~S delegating ~S to ~S~%"
                                     Me Any delegating-eval)
                        (send-message Eval
                          `(:do (:send-past ,delegating-eval
                                            ,Any ,R)
                                ,Env ,Id ,Gid ,Me)
                          C Eval))
                      Eval))
                  Eval)))))))
