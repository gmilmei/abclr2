;;; -*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system signal handler interface
;;; Wed Jul 27 12:34:00 JST 1994, by MASUHARA,H.

(in-package "ABCLR2-SYSTEM")

;;; The signal handler of ABCL/R2 consists of three layers: the ABCL
;;; handler, the Lisp interface, and the signal implementation.
;;;
;;; An event which is detected at the signal implementation layer is
;;; notified by calling a Lisp function or changing a Lisp variable,
;;; which is defined at the Lisp interface layer.  The ABCL/R2 runtime
;;; system delivers the event to an ABCL object specified by the user
;;; at the ABCL handler level.
;;;
;;; There could be various kinds of signals such as an I/O
;;; interruption or a timer interruption.  In this interface
;;; definition, these signals are "bundled" and delivered to a single
;;; ABCL object.  The kind of the signal is identified by arguments of
;;; a message to the handler object.

;;; The Lisp interface layer.
;;;
;;; Any signal is notifed by the one of the following ways.
;;;
;;; (1) Function (SEND-SIGNAL &rest arguments).  When this function is
;;; called, a message [:signal . arguments] is sent to a handler
;;; object eventually.
;;;
;;; (2) Variable *SIGNAL-REQUEST*.  When the value of this variable is
;;; changed to be a non-nil value, a message [:signal <the value>] is
;;; sent to handler object eventuallly.

;;; The ABCL handler layer.
;;;
;;; Any signal is notified by a message to an user-defined handler
;;; object.  The contents of the message depends on the arguments to
;;; the function or the value of the variable at the Lisp interface
;;; layer.

;;; Global variable *signal-handler*.  When a signal is raised at the
;;; Lisp interface layer, a message [:signal <arguments> ...] is sent
;;; to an object that is bound to this variable.

(defconstant *signal-handler-name* 'r2user::*signal-handler*)
(defconstant *signal-message-tag* ':signal)

(defvar *signal-request* nil
  "When a non-nil value is set to this variable, a signal message is
sent to a handler object.")

(defun send-signal-internal (message)
  (let ((target (global-get *signal-handler-name*)))
    (when target
      (send-message target message nil nil))))

(defun check-signal-request ()
  (when *signal-request*
    (let ((signal-value *signal-request*))
      (setq *signal-request* nil)
      (send-signal-internal (list *signal-message-tag* signal-value)))))

(defun send-signal (&rest arguments)
  (send-signal-internal
   (cons *signal-message-tag* arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions for predefined signals

(defmacro with-keyboard-interrupt-handler (&rest body)
  "The body is executed as usual.  When it is interrupted (e.g., by
Control-C), it sends a signal message."
  (let ((send-it '(send-signal :keyboard)))

    #+excl ;; Those who realizes interrupts as conditions.
    `(handler-bind
	 ((break #'(lambda (c)
		     (declare (ignore c))
		     ,send-it
		     (continue))))
       ,@body)

    #+:cmu ;; CMU common lisp provides a form WITH-ENABLED-INTERRUPTS.
    `(system:with-enabled-interrupts
	 ((unix:sigint #'(lambda (signal code scp)
			   (declare (ignore signal code scp))
			   ,send-it)))
       ,@body)

    #-(or :cmu excl) ;; Some Lisps can not capture the interrupt event.
                     ;; So far, KCL, AKCL, and MCL are the ones.
    `(progn ,@body)))
