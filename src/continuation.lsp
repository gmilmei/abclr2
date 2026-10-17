;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; continuation object
;;; Nov. 3, 1991 by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

;; (cont <pattern> <form> ...)
;; makes a continuation object.
(defmacro cont (pattern &rest body
			&aux (cont-obj (gensym))
			(M (gensym)) (R (gensym)) (S (gensym)))
  `(make-object-data
    :port #'(lambda (,cont-obj ,M ,R ,S)
	      (declare (ignore ,R ,S))
	      (match ,M
		(is ,pattern
		    ,@body)
		(otherwise
		 (warn "Continuation ~S cannot accept ~A"
		       ,cont-obj ,M))))
    :meta nil
    :den nil
    :gid nil
    :type 'Cont
    :status 'no-schedule
    :status-lock nil
    :name 'cont))
                       