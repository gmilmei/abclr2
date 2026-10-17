;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Incorporate&Compile
;;; Mar. 15, 1992 by MASUHARA, H.
;;; Jul. 29, 1992 context-sensitive inc and inc-lib.

(in-package "ABCLR2-SYSTEM")

(defmacro with-abclr2-style-warnings-muted (&body body)
  #+:SBCL
  `(handler-bind ((style-warning #'muffle-warning))
     ,@body)
  #-:SBCL
  `(progn ,@body))

(defun complete-abclr2-filename (partial-name)
  (merge-pathnames partial-name
		   (make-pathname :type abclr2-file-type
				  :defaults (pwd)
				  )))

(defun complete-lisp-filename (partial-name)
  (merge-pathnames partial-name
		   (make-pathname :type lisp-file-type
				  :defaults (pwd)
				  )))

(defun complete-fasl-filename (partial-name)
  (merge-pathnames partial-name
		   (make-pathname :type fasl-file-type
				  :defaults (pwd)
				  )))

(defun inc-file (name &key (verbose t) use-source)
  (let ((abclr2-file (complete-abclr2-filename name))
        (lsp-file (complete-lisp-filename name))
        (fasl-file (complete-fasl-filename name)))
    (unless (probe-file abclr2-file)
      (error "File ~S is not found." abclr2-file))
    (if (or use-source
            (not (probe-file lsp-file))
            (file-newer-p abclr2-file lsp-file))
      (translate-file abclr2-file lsp-file :verbose verbose))
    (if (and (not use-source)
             (probe-file fasl-file)
             (file-newer-p fasl-file lsp-file))
      (with-abclr2-style-warnings-muted
        (load fasl-file :verbose verbose))
      (with-abclr2-style-warnings-muted
        (load lsp-file :verbose verbose)))))

(defun comp (name &key (verbose t) use-source)
  (let ((abclr2-file (complete-abclr2-filename name))
        (lsp-file (complete-lisp-filename name))
        (fasl-file (complete-fasl-filename name)))
    (unless (probe-file abclr2-file)
      (error "File ~S is not found." abclr2-file))
    (if (or use-source
            (not (probe-file lsp-file))
	    (file-newer-p abclr2-file lsp-file))
      (translate-file abclr2-file lsp-file :verbose verbose))
    (if (or use-source
            (not (probe-file fasl-file))
	    (file-newer-p lsp-file fasl-file))
	(with-abclr2-style-warnings-muted
	  (compile-file lsp-file :output-file fasl-file))
	(format t "Fasl file ~S is up to date.~%"
		fasl-file))))

(defun translate-file (abclr2-file lsp-file &key (verbose t)
                                   &aux translation-ok)
  (if verbose 
      (format t "~&Translating ~S into ~S...~%" 
	      (enough-namestring abclr2-file (pwd))
	      (enough-namestring lsp-file (pwd))))
  (with-open-file (input-stream abclr2-file 
                                :direction :input)
    (unwind-protect
	 (with-open-file (output-stream lsp-file
					:direction :output
					:if-exists :supersede
					:if-does-not-exist :create)
	   (let ((*print-gensym* #+:SBCL t #-:SBCL nil) (*print-level* nil)
		 (*print-pretty* nil) (*print-length* nil)
		 exps)
             (format output-stream
                     ";; -*- Mode: Lisp; Package: ABCLR2-USER -*-~%~
                      (in-package \"ABCLR2-USER\")~%")
	     (do ((abclr2-exp (read input-stream nil :EOF nil)
			      (read input-stream nil :EOF nil)))
		 ((eq abclr2-exp :EOF))
	       (if verbose
		   (let ((*print-length* 3))
		     (format t "~&Translating ~S...~%"
			     abclr2-exp)))
	       (push (translate (parse abclr2-exp)) exps))
	     (let ((*package* (find-package :ABCLR2-USER)))
	       (write
		`(eval-when (:load-toplevel :execute)
		  (send-message ABCLR2-SYSTEM::*load-time-eval*
		   (list :do-progn (list ,@(nreverse exps))
		    ABCLR2-SYSTEM::*load-time-env*
		    ABCLR2-SYSTEM::*load-time-id*
		    ABCLR2-SYSTEM::*load-time-gid*
		    ABCLR2-SYSTEM::*load-time-eval*)
		   ABCLR2-SYSTEM::*load-time-cont*
		   ABCLR2-SYSTEM::*load-time-id*))
		:stream output-stream
		;; Preserve macro-generated lexical names and their shared identity.
		;; Printing #:NAME without reader labels would also lose identity.
		:circle #+:SBCL t #-:SBCL nil
		:escape t))
	     (terpri output-stream))
	   (setq translation-ok t))
      (unless translation-ok
        (when (probe-file lsp-file)
          (delete-file lsp-file))))))

#+COMMENT
(defun INCer-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me)))
   port (M C S)
   (declare (ignore S))
   ;;
   (if (not (consp M))
     (ABCLR2-ERROR "INCer object cannot handle a message ~S" M)
     (send-message C
		   (execute-and-catch-error 'inc
		    (apply #'inc-file M)) nil Me))))

#+COMMENT
(defun create-INCer (GMgr)
  (let ((INCer (make-object-data
                :port (create-queue)
                :meta nil
                :den nil
                :gid GMgr
                :type 'NRM
                :status 'dormant
                :name 'INCer)))
    (create-process #'INCer-loop INCer)
    INCer))

#+COMMENT
(defun COMPer-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me)))
   port (M C S)
   (declare (ignore S))
   ;;
   (if (not (consp M))
     (ABCLR2-ERROR "INCer object cannot handle a message ~S" M)
     (send-message C
		   (execute-and-catch-error
		    'comp
		    (apply #'comp-file M))
		   nil Me))))

#+COMMENT
(defun create-COMPer (GMgr)
  (let ((COMPer (make-object-data
                :port (create-queue)
                :meta nil
                :den nil
                :gid GMgr
                :type 'NRM
                :status 'dormant
                :name 'INCer)))
    (create-process #'COMPer-loop COMPer)
    COMPer))

#+:CCL
(defun choose-source-file (string)
  (let ((pathname (ccl::choose-file-dialog :mac-file-type "TEXT"
                                           :button-string string)))
    (make-pathname :directory (pathname-directory pathname)
                   :name (pathname-name pathname))))

#+COMMENT
(defmacro inc (&rest args)
  #+:CCL
  `(bracket (create-INCer group) 
            <==
            ,(if (null args)
               `(list (choose-source-file "Incorporate") :verbose t)
               `',args))
  #-:CCL
  `(bracket (create-INCer group) <== ',args))

#+COMMENT
(defmacro comp (&rest args)
  #+:CCL
  `(bracket (create-COMPer group) <==
            ,(if (null args)
               `(list (choose-source-file "Compile") :verbose t)
               `',args))
  #-:CCL
  `(bracket (create-COMPer group) <== ',args))

;;;
(defun io-client-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me)))
   port (M C S)
   (declare (ignore S))
   ;;
   (if (not (listp M))
     (ABCLR2-ERROR "IO-client object cannot handle a message ~S" M)
     (execute-and-catch-error 'IO-client
			      (apply (first M) C (rest M)))
     )))

(defun create-io-client (GMgr)
  (let ((io-client (make-object-data
		    :port (create-queue)
		    :meta nil
		    :den nil
		    :gid GMgr
		    :type 'NRM
		    :status 'dormant
		    :name 'IO-Client)))
    (create-process #'io-client-loop io-client)
    io-client))

;;;

(defmacro inc-lib (&rest args &aux
			 (C (gensym)) (parameters (gensym)) (Id (gensym)))
  `(bracket (create-io-client group) <==
    (list #'(lambda (,C ,parameters ,Id)
	      (inc-lib-main ,parameters ,C ,Id))
     (list ,@args) Me)))
     

(defun inc-lib-main (parameters C Id)
  (inc-main parameters C *global-env* Id *group* (global-get '*eval*)))
;;;

(defun ask-file-name-iteractively (mode-name)
  #+:CCL(choose-source-file mode-name)
  #-:CCL(progn (format t "~&File name to ~A> " mode-name)
	       (read-line)))

(defun parse-inc-parameter (parameters mode-name)
  (apply #'(lambda (&optional (file-name
			       (ask-file-name-iteractively mode-name))
			      &key use-source (verbose t) compile)
	     (values file-name use-source verbose compile))
	 parameters))

(defun inc-main (parameters C Env Id Gid Eval)
  ;; parse parameters
  (multiple-value-bind (name use-source verbose compile)
      (parse-inc-parameter parameters "incorporate")
    ;; Compile before incorporation if designated.
    (when compile
      (comp name :use-source use-source :verbose verbose))
    ;; Pathnames are created from given file-name.
    (let ((abclr2-file (complete-abclr2-filename name))
	  (lsp-file (complete-lisp-filename name))
	  (fasl-file (complete-fasl-filename name)))
      ;; Existence of the source file is checked.
      (unless (probe-file abclr2-file)
	(error "File ~S is not found." abclr2-file))
      ;; If the source file is new, translate it first.
      (if (or use-source
	      (not (probe-file lsp-file))
	      (file-newer-p abclr2-file lsp-file))
	  (translate-file abclr2-file lsp-file :verbose verbose))
      ;; Load time contexts are set.
      (let ((*load-time-cont* C) (*load-time-env* Env)
	    (*load-time-id* Id) (*load-time-gid* Gid)
	    (*load-time-eval* Eval))
	;; If there exists compiled file, load it.
	(if (and (not use-source)
		 (probe-file fasl-file)
		 (file-newer-p fasl-file lsp-file))
	    (load fasl-file :verbose verbose)
	    (load lsp-file :verbose verbose))))))
