;;; -*- Mode: Common-Lisp; Package: ABCLR2-SYSTEM -*-
;;; signal-handler.lisp --- default signal handler
;;; Wed Jul 27 20:47:23 JST 1994, by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

#|;; Original definition
[object *default-signal-handler*
 (meta-gen non-reifying-meta)
 (script
  (=> [:signal . parameters]
      (format t "[Signal ~S is ignored]" parameters)))]
|#

(defun default-signal-handler-loop (Me)
  (primary-NRM-loop Me env nil (M R S)
   (declare (ignore R S))
   (match M
	    (is (bracket* :signal parameters)
		(format t "[Signal ~S is ignored]" paraMeters))
	    (otherwise
	     (warn "object ~S cannot handle the Message ~S" (den-of Me)
		   (second M))))))

(defun create-default-signal-handler (&optional Group)
  (let ((handler (make-object-data
		  :port (create-queue)
		  :meta nil
		  :den nil
		  :gid Group
		  :type 'NRM
		  :status 'dormant
		  :name 'r2user::*DEFAULT-SIGNAL-HANDLER*)))
    (create-process #'default-signal-handler-loop handler)
    handler))