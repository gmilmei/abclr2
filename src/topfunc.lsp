;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM-*-
;;; topfunc.lsp
;;; Functions used by toplevel modules
;;; Create: Jul 5, 1992, by MASUHARA, H.
;;; Update: Oct 8, 1993, cd and pwd for ACL
;;; Update: May11, 1994, cd and pwd for CLISP
;;;
;;; Functions used at toplevel.

(in-package "ABCLR2-SYSTEM")

(defun file-newer-p (p1 p2)
  (> (file-write-date p1) (file-write-date p2)))

#+COMMENT
(defun get-default-path ()
  #+:LUCID (pwd)
  #-:LUCID (global-get '*default-pathname-defaults*))

(defun global-set (variable value)
  (send-message *global-env* (list :set variable value) nil nil))
(defun global-get (variable)
  (send-present-message *global-env* (list :value-of variable) nil))
(defun global-boundp (variable)
  (send-present-message *global-env* (list :bonudp variable) nil))

#-:LUCID
(defun pwd ()
  "Print current pathname"
  #+(or KCL :CMU)
  (format nil "~A/" (namestring (truename ".")))
  #+:CCL
  (directory-namestring (truename ":"))
  #+excl
  (excl:current-directory)
  #+clisp
  (cd)
)

#-(or :LUCID clisp)
(defun cd (&optional pathname)
  (unless pathname
    (setq pathname
          #-:CCL
	  (directory-namestring (truename (user-homedir-pathname)))
          #+:CCL
          (ccl:choose-directory-dialog)))
  #+:CMU
  (unix:unix-chdir pathname)
  #+KCL
  (system:chdir pathname)
  #+:CCL
  (ccl:set-mac-default-directory pathname)
  #+excl
  (excl:chdir pathname)
  (pwd)
)

#|
#+KCL
(defun getenv (env-string) (system:getenv env-string))

#+:CMU
(defun getenv (env-string) 
  (cdr (assoc (intern env-string 'keyword) extensions:*environment-list*)))

#+:CCL
(defun getenv (env-string) 
  (declare (ignore env-string))
  (error "Function GETENV is not supported"))

;; LUCID Common Lisp has cd and pwd commands

#-:LUCID
(defun cd (pathname)
  (setq *default-pathname-defaults* 
        (merge-pathnames pathname  
                          (global-get '*default-pathname-defaults*)))
  (global-set '*default-pathname-defaults*
             *default-pathname-defaults*)
  (pwd))

#-:LUCID
(defun pwd () (pathname-directory (global-get '*default-pathname-defaults*)))
|#

