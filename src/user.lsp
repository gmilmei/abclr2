;;;-*- Mode: Lisp; Package: ABCLR2-USER -*-
;;; user.lsp
;;; ABCL/R2 user level functions and macros
;;; Create: Jul. 5, 1992 by MASUHARA, H.
;;; Update: Jul.29, 1992 --- require&provide
;;; Update: Jul.31, 1992 --- bug of DO in defun is fixed.
;;; Update: Jan. 8, 1993 --- Function gc-verbose is defined.
;;; Update: Mar.26, 1993 --- LOOP is redefined as a macro to DO.
;;; Update: Oct. 8, 1993 --- shadow statement is moved to defabclr2.lsp.
;;; Update: Oct. 8, 1993 --- gc-verbose for ACL
;;; Updaet: Mar.11, 1994 --- gc-verbose for silent lisps.

(in-package "ABCLR2-USER")

#+COMMENT
(eval-when (compile load eval)
  (shadow '("DOTIMES" "DOLIST" "TIME" "PROVIDE" "REQUIRE" "LOOP")))

;;
;; DOLIST and DOTIMES
;; DOLIST is a macro to DO because TAGBODY is not available.
(defmacro dolist (control &rest body)
  "Common Lisp compatible DOLIST."
  (let ((dovar (gensym))
        (itemvar (first control))
        (initform (second control))
        (resultforms (rest (rest control))))
  `(do ((,dovar ,initform (rest ,dovar))
        (,itemvar))
       ((null ,dovar) ,@resultforms)
     (setq ,itemvar (first ,dovar))
     ,@body)))

;; DOTIMES is a macro to DO because TAGBODY is not available.
(defmacro dotimes (control &rest body)
  "Common Lisp compatible DOTIMES."
  (let ((finalcount (gensym))
        (counter (first control))
        (times (second control))
        (resultforms (rest (rest control))))
    `(do ((,counter 0 (1+ ,counter))
          (,finalcount ,times))
         ((>= ,counter ,finalcount) ,@resultforms)
       ,@body)))

;; LOOP is defined as a macro that expands to DO form because TAGBODY is not
;; available.
;; BTW, who uses LOOP forms in ABCL/R2, which does not support RETURN-FROM?
(defmacro loop (&rest body)
  "Common Lisp compatible LOOP."
  `(do () (nil)
    ,@body))

;;
;; Reset an object:
;;	This is implemented simply by sending a `:end' message to
;;	a metaobject of a given object.
;;
(defmacro reset (object)
  "Force an object to be dormant."
  `(bracket (bracket meta ,object) <= :end))

;;
;; Requrie & Provide
;;
(defmacro provide (module-symbol)
  "Provide an ABCL/R2 module"
  `(let ((module-name
	  (etypecase ,module-symbol
	    (symbol (string-downcase (symbol-name ,module-symbol)))
	    (string (string-downcase ,module-symbol)))))
    (pushnew module-name *modules*)))

(defun require-find-pathname-to-incorporate 
       (module-symbol library-directories loaded-modules)
  (let ((module-name
         (typecase module-symbol
           (symbol (string-downcase (symbol-name module-symbol)))
           (string module-symbol)
           (t (error "Module name should be string or symbol, but~
                      supplied ~S" module-symbol)))))
    (and (not (member module-name loaded-modules
                      :test #'string=))
         (let ((found-directory
                (find-if
                 #'(lambda (directory)
                     (probe-file 
                      (make-pathname :directory (pathname-directory directory)
                                     :name module-name
                                     :type r2sys::abclr2-file-type)))
                 library-directories)))
           (and found-directory
                (make-pathname :directory (pathname-directory found-directory)
                                     :name module-name))))))
    
(defmacro require (module-symbol &key use-source (verbose t) compile)
  "Require an ABCL/R2 module"
  `(let ((pathname (require-find-pathname-to-incorporate 
                    ,module-symbol *library-directories* *modules*)))
     (if pathname
       (inc-lib pathname :use-source ,use-source
                :verbose ,verbose
                :compile ,compile))))

;;
;; atomic form
;;
(defmacro atomic (&rest body)
  "evaluates body expressions with a guarantee that the evaluation
is not took place with other atomic form simulataneously."
  (let ((result (gensym)))
    `(let ((,result (progn
		      (bracket (bracket meta me) <== :lock)
		      ,@body)))
      (bracket (bracket meta me) <= :release)
      ,result)))

;;
;; for future type messages
;;
(defmacro make-future () 
  (let ((future-object (gensym)))
    `(let ((,future-object (bracket future-object-generator <== :new)))
       (bracket ,future-object <= :reset)
      ,future-object)))
(defmacro reset-future (future-object)
  `(bracket ,future-object <= :reset))
(defmacro ready? (future-object)
  `(bracket ,future-object <== :ready?))
(defmacro next-value (future-object &key (remove t))
  `(bracket ,future-object <== (bracket :next-value ,remove)))
(defmacro all-values (future-object &key (wait t) (remove t))
  `(bracket ,future-object <== (bracket :all-values ,wait ,remove)))

;;
;; GC messages
;;
(defun gc-verbose (Boolean)
  #-(or KCL :CMU excl) (declare (ignore Boolean))
  #+KCL (setf (symbol-value 'system:*notify-gbc*) Boolean)
  #+:CMU (setf (symbol-value 'extensions:*gc-verbose*) Boolean)
  #+excl (setf (sys:gsgc-switch :print) Boolean
	       (sys:gsgc-switch :stats) Boolean
	       (sys:gsgc-switch :verbose) Boolean))

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;;; If every Lisp systems were implemented a TIME form by a macro,
;;; TIME in ABCL/R2 would be also implemented by a macro.

#+:CCL-2
(defmacro time (form)
  (let ((tmp1 (gensym)) (tmp2 (gensym)) (tmp3 (gensym)) (tmp4 (gensym))
        (result (gensym)))
    `(let (,tmp1 ,tmp2 ,tmp3 ,tmp4 ,result)
       (setq ,tmp1 (ccl::%get-long (ccl::%get-ptr (ccl::%int-to-ptr 2308))
		    -310))
       (setq ,tmp2 (ccl::%get-long (ccl::%int-to-ptr 362)))
       (setq ,tmp3 (ccl::%get-long (ccl::%get-ptr (ccl::%int-to-ptr 2308))
		    -348))
       (setq ,tmp4 (ccl::total-bytes-allocated))
       (setq ,result ,form)
       (ccl::report-time
        ',form
        (- (ccl::%get-long (ccl::%int-to-ptr 362)) ,tmp2)
        (- (ccl::%get-long (ccl::%currenta5) -348) ,tmp3)
        (- (ccl::%get-long (ccl::%currenta5) -310) ,tmp1)
        (- (ccl::total-bytes-allocated) ,tmp4))
       ,result)))

#+KCL
(defmacro time (form)
  `(let (system::real-start system::real-end system::run-start
	 system::run-end system::x)
    (setq system::real-start (get-internal-real-time))
    (setq system::run-start (get-internal-run-time))
    (setq system::x ,form)
    (setq system::run-end (get-internal-run-time))
    (setq system::real-end (get-internal-real-time))
    (fresh-line *trace-output*)
    (format *trace-output*
     "Evaluation of ~S took:~%~
      real time : ~,3F secs~%~
      run time  : ~,3F secs~%"
     ',form
     (/ (- system::real-end system::real-start)
      internal-time-units-per-second)
     (/ (- system::run-end system::run-start)
      internal-time-units-per-second))
    system::x))

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#