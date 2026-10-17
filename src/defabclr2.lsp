;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;; defr2.lsp
;;; ABCL/R2 initial order
;;; Create: Jul. 5, 1992 by MASUHARA, H.
;;; Update: Jul.26, 1992 --- dump routine for MCL.
;;; Update: Aug.13, 1992 --- use-packages for Sun-CL
;;; Update: Oct. 7, 1993 --- shadowing statement is moved from user.lsp
;;; Update: Oct. 8, 1993 --- package and dump for ACL
;;; Update: May.11, 1994 --- package and dump for CLISP
;;; Update: Jul. 7, 1995 --- new defsystem for ACL 4.2

;;;
;;; site specific information
;;;
(defvar *R2SYS-SOURCE-PATHNAME* "./"
  "Directory name where source files of ABCL/R2 system is stored.")
(defvar *R2SYS-BINARY-PATHNAME* "./"
  "Directory name where binary files of ABCL/R2 system is saved.")
(defvar *R2SYS-DUMP-PATHNAME* "saved_abclr2"
  "Pathname which ABCL/R2 system is dumped.")
(defvar *default-abclr2-library-pathnames*
  (list "")				; default: current directory only
  "List of pathnames for default library directories.")

;;;
;;; package definition
;;;
;;; Package named ABCLR2-SYSTEM is for the system codes.
;;; Package named ABCLR2-USER is for the user programs which needs
;;; isolated namespace for functions and macros.
;;;

#+:SBCL
(unless (find-package "LISP")
  ;; The bundled 1995 defsystem uses the pre-ANSI package name LISP.
  ;; Keep its PROVIDE/REQUIRE implementation separate from COMMON-LISP.
  (let ((package (make-package "LISP" :use '("COMMON-LISP"))))
    (shadow '(provide require *modules*) package)
    (export (mapcar #'(lambda (name) (find-symbol name package))
                    '("PROVIDE" "REQUIRE" "*MODULES*"))
            package)
    (setf (symbol-function (find-symbol "PROVIDE" package)) #'provide
          (symbol-function (find-symbol "REQUIRE" package)) #'require
          (symbol-value (find-symbol "*MODULES*" package)) nil)))

#+:SBCL
(unless (find-package "USER")
  (let ((package (find-package "COMMON-LISP-USER")))
    (rename-package package (package-name package)
                    (cons "USER" (package-nicknames package)))))

#+:SBCL
(pushnew :cltl2 *features*)

#+(or KCL clisp)			; CLtL1
(progn
  (unless (find-package "ABCLR2-SYSTEM")
    (make-package "ABCLR2-SYSTEM"
		  :nicknames '("R2SYS")
		  :use '("LISP")))
  (unless (find-package "ABCLR2-USER")
    (make-package "ABCLR2-USER"
		  :nicknames '("R2USER")
		  :use '("R2SYS" "LISP"))))

#+:LUCID				; almost CLtL2, but...
(progn
  (unless (find-package "ABCLR2-SYSTEM")
    (make-package "ABCLR2-SYSTEM"
		  :nicknames '("R2SYS")
		  :use '("LISP" "LUCID-COMMON-LISP")))
  (unless (find-package "ABCLR2-USER")
    (make-package "ABCLR2-USER"
		  :nicknames '("R2USER")
		  :use '("R2SYS" "LISP" "LUCID-COMMON-LISP"))))

#+(or :CMU :CCL excl :SBCL)		; CLtL2
(progn
  (unless (find-package "ABCLR2-SYSTEM")
    (make-package "ABCLR2-SYSTEM"
		  :nicknames '("R2SYS")
		  :use '("COMMON-LISP")))
  (unless (find-package "ABCLR2-USER")
    (make-package "ABCLR2-USER"
		  :nicknames '("R2USER")
		  :use '("R2SYS" "COMMON-LISP"))))

#+excl
(import '(cltl1::special-form-p) :r2sys)

#+:SBCL
(defun r2sys::special-form-p (symbol)
  (special-operator-p symbol))

(shadow '(r2user::dotimes r2user::dolist r2user::time r2user::provide
	  r2user::require r2user::loop) :r2user)
(use-package '(:r2sys
	       #+(or KCL :LUCID clisp) :lisp
	       #+:LUCID :lucid-common-lisp
	       #+(or :CMU :CCL excl :SBCL) :common-lisp)
	     :r2user)

;;;
;;; defsystem.lisp is loaded if it has not been.
;;;
(unless (find-package "MAKE")
  (load (merge-pathnames 
	 ;; Fri Jul  7 19:12:41 JST 1995
	 ;; Compatibility of the defsystem package and 
	 ;; Lisp systems are subtle.
	 ;; ACL 4.2 is incompatible with the old defsystem,
	 ;; but the (new) defsystem 30 is not incompatible with
	 ;; AKCL.
	 #+(or :allegro-v4.2 :allegro-v5.0 :CMU :SBCL) "defsystem30.lsp"
	 #-(or :allegro-v4.2 :allegro-v5.0 :CMU :SBCL) "defsystem.lsp"
         *R2SYS-SOURCE-PATHNAME*)))

(push *R2SYS-SOURCE-PATHNAME* make:*central-registry*)

;;;
;;; dump the system
;;;
(defun dump-system (pathname)
  #+KCL
  (progn
   (gbc t)
   (defun system:top-level ()
     (in-package :r2user)
     (r2sys::startup))
   (si:save-system pathname)
   ;; (quit)
   )
   
  #+:CMU
  (progn
    (gc)
    (save-lisp pathname
	     :init-function #'(lambda ()
				(in-package :r2user)
				(r2sys::startup)
				(cl-user::quit))
	     :purify t
	     :print-herald nil)
    ;;(quit)
    )

  #+:SBCL
  (progn
    (gc :full t)
    (sb-ext:save-lisp-and-die
     pathname
     :toplevel #'(lambda ()
                   (in-package :r2user)
                   (r2sys::startup)
                   (sb-ext:exit))
     :executable t))
  #+:LUCID
  (progn
    (disksave pathname
	      :restart-function #'(lambda ()
				    (in-package :r2user)
				    (r2sys::startup)
				    (user::quit))
	      :full-gc t)
    ;; (quit)
    )

  #+(and :CCL (not :CCL-2))
  (dumplisp pathname
	    :toplevel-function
	    #'(lambda ()
		(in-package :r2user)
		(r2sys::startup)))
  
  #+:CCL-2
  (save-application pathname
                    :toplevel-function
                    #'(lambda ()
                        (in-package :r2user)
                        (r2sys::startup)
                        (ccl::quit))
                    :creator "ABCL")
  
  #+excl
  (dumplisp :name pathname
	    :restart-function
	    #'(lambda () 
		(in-package :r2user)
		(r2sys::startup)
		(excl:exit)))

  #+clisp
  (progn
    (do-all-symbols (sym) (remprop sym 'sys::definition))
    (setq - nil + nil ++ nil +++ nil * nil ** nil *** nil / nil // nil /// nil)
    (savemem pathname)
    (exit))
  )

;;;
;;; make the system
;;;
(defun build-and-dump (&optional (pathname *R2SYS-DUMP-PATHNAME*))
  (compile-system 'ABCLR2)
  (dump-system pathname))
