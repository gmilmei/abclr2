;;;; init.lsp for univ-port


;;;; switches

(defparameter j "This is KCL.")

(setq system::*ignore-eof-on-terminal-io* t)

(setq *print-case* :downcase)
(setq *print-circle* t)


;;;; N times

(defmacro 5times (&rest forms)
  (let ((newvar (gensym)))
       `(dotimes (,newvar 5) ,@ forms)))

(defmacro 10times (&rest forms)
  (let ((newvar (gensym)))
       `(dotimes (,newvar 10) ,@ forms)))

(defmacro 100times (&rest forms)
  (let ((newvar (gensym)))
       `(dotimes (,newvar 100) ,@ forms)))

(defmacro 1000times (&rest forms)
  (let ((newvar (gensym)))
       `(dotimes (,newvar 1000) ,@ forms)))

(defmacro 5000times (&rest forms)
  (let ((newvar (gensym)))
       `(dotimes (,newvar 5000) ,@ forms)))


;;;; other utility

(defmacro def-macro-synonym (new old)
  `(defmacro ,new (&rest args)
	     (list* ',old args)
	     ))

(defun sf (symbol) (symbol-function symbol))
(defun sp (symbol) (symbol-plist symbol))
(defun plist (symbol) (symbol-plist symbol))

(defmacro pp (x)
  (if (symbolp x)
      `(symbol-function ',x)
      `(symbol-function ,x)))

(defun macro-test ()
  (let (s)
       (loop (terpri)
	     (princ "expand1> ")
	     (setq s (read))
	     (when (member s '(:q q quit bye exit))
		   (return (values)))
	     (format t "~S~%" (macroexpand-1 s)))))

(def-macro-synonym test macro-test)

(defun fact (n)
  (if (zerop n) 1 (* n (fact (1- n)))))

(defun compfile (file)
  (compile-file file :o-file t :c-file t :h-file t :data-file t)
  (beep))

(defun beep () (princ "") (values))
(defun b    () (princ "") (values))

;;;

(defun script (&optional (file-name "temp"))
  (dribble file-name)
  ;(format t "Script started, file is ~s" file-name)
  (values))

(defun endscript ()
  ;(format t "Script done, file is ~s" file-name)
  (dribble))

(def-macro-synonym end endscript)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TOP-1 common Lisp specific

#+(and KCL LUNA88K)
(progn
;;;; future
  
  (proclaim '(optimize (safety 2) (space 3)))	; need this for futures
  
;;;                   c vs frs ihs
  (defvar *stacksize* '(2000 1000 64 128))
  
  (defvar *normal-stack* '(2000 1000 512 512))
  
;;; If you don't use ABCL/1,
;;; please replace *large-stack* with *stacksize* in 'future'
  (defvar *large-stack* '(5000 5000 1024 1024))
  
  (defvar *special-stack* '(20000 20000 2048 2048))
  ;; this caused "Process contexts exhausted". 12/03 1990
  ;; because c or vs is too large.
  
  (defvar *priority* 0)
  
  ;; for debugging
  
  (defmacro future (form &aux (newvar (gensym)))
    `(let ((,newvar (si:make-future)))
      (si:mv-on ,newvar)
      (si:process-funcall *stacksize* t *priority*
       #'(lambda () (eval-set-future ,newvar ,form)))
      ,newvar))
  #+comment
  (defmacro future (form &aux (newvar (gensym)))
    ;; for abcl
    `(let ((,newvar (si:make-future)))
      (si:mv-on ,newvar)
      (si:process-funcall *large-stack* t *priority*
       #'(lambda () (eval-set-future ,newvar ,form)))
      ,newvar))
  
  #+comment
  (defmacro future (form &aux (newvar (gensym)))
    ;; original definition of 'future'
    `(let ((,newvar (si:make-future)))
      (si:mv-on ,newvar)
      (si:process-funcall *stacksize* t *priority*
       #'(lambda () (eval-set-future ,newvar ,form)))
      ,newvar))
  
  (defmacro fork (&rest forms)
    `(si:process-funcall *stacksize* t *priority*
      #'(lambda () (progn ,@ forms))))
  
  
;;;; mp-related utility
  
  (defun np () (si:proc-in-runq))
  (defun lap () (si:list-all-processes))
  
  (defmacro runq-empty-p ()
    '(zerop (si:proc-in-runq)))
  
  (defvar *fc* 0)
  (defun set-fc () (setq *fc* (si:future-counter)))
  (defun fc ()
    (format t "~%current fc:  ~S" (si:future-counter))
    (format t "~%fc increase: ~S" (- (si:future-counter) *fc*))
    (values))
  
  
;;;; time
  
  (setf (symbol-function 'kcl-time) (symbol-function 'time))
  
  #+comment
  (defmacro time (form &aux (t1 (gensym)) (t2 (gensym)) (tval (gensym)))
    `(let (,t1 ,t2 ,tval gc-count gc-time)
      (setq gc-count (si:seq-gc-count))
      (setq gc-time (si:seq-gc-accum-time))
      (setq ,t1 (si:top1time))
      (setq ,tval (multiple-value-list ,form))
      (setq ,t2 (si:top1time))
      (let* ((total (- ,t2 ,t1))
	     (gc (- (si:seq-gc-accum-time) gc-time))
	     (wogc (- total gc)))
	(format t "~%without gc: ~,3F secs" (/ wogc 1000))
	(format t "~%gc count  : ~S" (- (si:seq-gc-count) gc-count))
	(format t "~%total     : ~,3F secs"  (/ total 1000))
	)
      (values-list ,tval)))

  #+KCL
  (defmacro time (form &aux (start (gensym)) (val (gensym)) (stop (gensym)))
    `(let (,start ,val ,stop)
      (setq ,start (get-universal-time))
      (setq ,val (multiple-value-list ,form))
      (setq ,stop (get-universal-time))
      (format t "time: ~S secs" (- ,stop ,start))
      (values-list ,val)))
  
;;;; handler for control-C
;;; The following should be executed in "init.lsp"
  
;;; modified for execution of TOP1 ABCL/1 in 11/20 1990
;;; original version of init.lsp is kept as init.old
  
;;; if plain kcl (not for executing ABCL/1) is needed,
;;; please change 'is-abcl' as  'nil'
  
  (defvar is-abcl nil)
  
  (defvar here t)
  (setq here (directory "init.lsp"))
  (if (and (consp here)
	   (equal (car here) (pathname "/home/camille1/ytake/src/abcl1/init.lsp")))
      (progn
	(setq is-abcl t)
	(format t "~%Control-C handler for ABCL/1 started.")
	(values))
      (setq is-abcl nil))
  (defvar is-abort nil)
  
  (setq *main-process* (si:process-self))
  
  (defun control-c-handler ()
    ;; the daemon process
    (let ((self (si:process-self)))
      (if is-abcl
	  ;; control-c handler for abcl/1
	  ;; every process checks 'is-abort' in every loop
	  (loop
	   (si::wait-system-signal 2)
	   (setq is-abort t))
	  
	  (loop
	   (si:wait-system-signal 2)
	   (dolist (p (si:list-all-processes))
	     (or (eq p *main-process*)
		 (eq p self)
		 (si:process-interrupt p #'si:process-exit)))
	   (si:process-interrupt *main-process* #'si:terminal-interrupt t)))))
  
  (defvar *control-c-handler-daemon-process*
    (prog1 (si:process-funcall '(1000 500 16 16) t 0 #'control-c-handler)
      (format t "~%control-c-handler daemon started.")))
  
  ;; (load "semaphore")
  )
;;; End of TOP-1 common Lisp specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(beep)

