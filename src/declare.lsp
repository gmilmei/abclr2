;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; declare.lsp
;;; Variable and constants declaration
;;; Create:Jul. 5, 1992: by MASUHARA, H.
;;; Update:Jul.17, 1992: by MASUHARA, H. for future type messages.
;;; Update:Jul.19, 1992: by MASUHARA, H. treatment of *eval*
;;; Update:Jul.25, 1992: by MASUHARA, H. script with FROM field.
;;; Update:Jul.27, 1992: by MASUHARA, H. script with WHERE field.
;;; Update:Jul.29, 1992: by MASUHARA, H. for require&provide
;;; Update:Aug.13, 1992: by MASUHARA, H. pacakges for Sun-CL
;;; Update:Sep.27, 1992: by MASUHARA, H. _ is added to export list
;;; Update:Nov.16, 1992: by MASUHARA, H. version info. updated
;;; Update:Dec.11, 1992: by MASUHARA, H. GROUP-OF from.
;;; Update:Jan. 8, 1993: by MASUHARA, H. RESOURCES exported.
;;; Update:Mar.23, 1993: by MASUHARA, H. prompt changed in MCL
;;; Update:Jun.13, 1993: by MASUHARA, H. reify-args is added
;;; Update:Oct. 8, 1993: by MASUHARA, H. for ACL
;;; Update:Mar.11, 1994: by MASUHARA, H. for CLISP
;;; Update:Dec.12, 1994: by MASUHARA, H. around extras for object creation.
;;; Update:Dec.14, 1994: by MASUHARA, H. improved where fields in scripts

(in-package "ABCLR2-SYSTEM")

(export '(object meta-gen group state script non-reifying-meta => @ $
          evaluator meta den me startup exclam bracket bracket* <= <==
          inc comp match is otherwise find-script search-script _
          remove-script parse-exp call-script-body statespec-initforms
          statespec-varnames queue-gen env-gen global-env *print-time*
          cont cd pwd dormant queue scriptSet mode temporary *eval* from
	  where heavy-meta inc-lib group-of create-primary-eval
	  create-primary-meta-gen resources reify-args reify-args-to-meta
	  *signal-request* send-signal))

#+(or :CMU :CCL-2 excl :SBCL)		; CLtL2
(import '(CL-USER::*default-abclr2-library-pathnames*))
#+(or :KCL (and :CCL (not :CCL-2)) :LUCID clisp) ; CLtL1
(import '(USER::*default-abclr2-library-pathnames*))

#+:SBCL
(defparameter *ABCLR2-VERSION*
  "ABCL/R2 Gesi version (Jul. 7, 1995, 0.59)")
#-:SBCL
(defconstant *ABCLR2-VERSION*
  "ABCL/R2 Gesi version (Jul. 7, 1995, 0.59)")

(defvar *ABCLR2-PROMPT* #-:CCL"~&ABCL/R2>" #+:CCL ""
  "Prompt string for toplevel loop.")

#+:SBCL
(defparameter abclr2-file-type "abclr2"
  "File type for ABCL/R2 source files.")
#-:SBCL
(defconstant abclr2-file-type "abclr2"
  "File type name for ABCL/R2 programs")
#+:SBCL
(defparameter lisp-file-type "lsp"
  "File type for Lisp source files.")
#-:SBCL
(defconstant lisp-file-type "lsp"
  "File type name for files that are translated from ABCL/R2 file.")
(defconstant fasl-file-type (cdr make::*filename-extensions*))

#+COMMENT
(defparameter dumpimage-pathname "saved_abclr2")

(defvar *break-illegal-message* nil
  "Stop primary kernel objects when they received ~
   unexecutable messages.")
(defvar *abclr2-system-debug* *error-output*
  "Outputstream for ABCL/R2 system debugging.")
(defvar *global-env* nil
  "An environment used for primary objects and toplevel.")
(defvar *toplevel* nil
  "The read-eval-print object")
;;(defvar *eval* nil
;;  "Default and primary evaluator")
(defvar *group* nil
  "Default and primary group manager")
(defvar *queue-gen* nil
  "Primary queue generator")
(defvar *nrm-gen* nil
  "Generator of non-reifying metaobject. It is bound as
NON-REIFYING-META in *global-env*.")
(defvar *env-gen* nil
  "Primary environment generator")

;; Context information for translated ABCL/R2 programs.
(defvar *load-time-cont* nil)
(defvar *load-time-env* nil)
(defvar *load-time-id* nil)
(defvar *load-time-gid* nil)
(defvar *load-time-eval* nil)
