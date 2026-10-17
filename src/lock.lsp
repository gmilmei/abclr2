;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; lock primitive
;;; Nov. 17, 1991 by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

;; (make-lock)
;; (lock lock-object)
;; (unlock lock-object)

(defun make-lock ()
  #+KCLM (si:make-lock)
  #-KCLM nil)
(defun lock  (lock-object)
  #+KCLM (si:process-lock lock-object)
  #-KCLM t)
(defun unlock (lock-object)
  #+KCLM (si:process-unlock lock-object)
  #-KCLM t)