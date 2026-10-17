;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; catch errors
;;; Dec. 16, 1991 by MASUHARA, H.
;;; Jul. 21, 1992 --- misinterpreting multiple-values as an error is fixed.
;;; Oct.  8, 1993 --- definitions for ACL
;;; May. 11, 1994 --- definitions for CLISP

(in-package "ABCLR2-SYSTEM")

(defvar *abclr2-error* *error-output*)
(defparameter enter-lisp-error nil)

(defun enter-lisp-error (flag)
  (setq enter-lisp-error flag)
  #+:CCL
  (setq ccl::*break-on-errors* flag))

(defun enter-lisp-error-p () enter-lisp-error)

#+KCL
(defmacro execute-and-catch-error (call-type &rest body)
  `(if enter-lisp-error
    (let ((*break-enable* t))
      ,@body)
    (let (#+COMMENT
	  (*break-enable* nil)
	  (error-flag t)
	  (val nil))
      (catch system::*quit-tag*
	(setq val (progn ,@body))
	(setq error-flag nil))
      (if error-flag
	  (format *abclr2-error* "~&;An error is deteceted.~
                                  ~&;During ~A.~%" ,call-type))
      (values val error-flag))))

#+(or :CMU :LUCID :CCL-2 excl :SBCL);; and other Lisps which have condition handlers.
(defmacro execute-and-catch-error (call-type &rest body)
  `(if enter-lisp-error
    (progn ,@body)
    (multiple-value-bind (val error-obj)
	(ignore-errors (multiple-value-bind (result)
			   (progn ,@body)
			 result))
      (if error-obj
	  (progn (format *abclr2-error* "~&;An error is detected.~
                                         ~&;~A~
                                         ~&;During ~A.~%" error-obj ,call-type)
		 #+COMMENT(error error-obj)
		 ))
      (values val error-obj))))

#+(and :CCL (not :CCL-2))
(defmacro execute-and-catch-error (call-type &rest body)
  `(if enter-lisp-error
    (progn ,@body)
    (multiple-value-bind
	  (val error-p) (catch-error ,@body)
      (if error-p
	  (format *abclr2-error* "~&;An error is detected.~
                                  ~&;~S~
                                  ~&;Under ~A.~%"
		  (apply #'format nil val)
		  ,call-type))
      (values val error-p))))

#+clisp
(defmacro execute-and-catch-error (call-type &rest body)
  `(if enter-lisp-error
     (progn ,@body)
    ,(let ((blockvar (gensym)))
       `(block ,blockvar
	 (let ((*error-handler*
		#'(lambda (&rest args)
		    (format *abclr2-error* "~&;An error ~? is detected.~
                                           ~&;During ~A.~%"
			    (second args) (rest (rest args))
			    ,call-type)
		    (return-from ,blockvar (values nil t)))))
	   (values
	    (progn ,@body) nil))))))

#+COMMENT
(defmacro execute-and-catch-error (call-type &rest body)
  (declare (ignore call-type))
  `(progn ,@body))

;; print error messages, then return.
(defun abclr2-error (&rest formats)
  (if enter-lisp-error 
    (apply #'error formats)
    (let ((*print-level* 2)
	  (*print-length* 3))
      (format *abclr2-error* "~&ABCLR2-ERROR:~A~%"
	      (apply #'format nil formats)))))

;; print warnig, then return.
(defun abclr2-warn (&rest formats)
  (let ((*print-level* 2)
	(*print-length* 3))
    (format *abclr2-error* "~&ABCLR2-WARN:~A~%"
	    (apply #'format nil formats))))
