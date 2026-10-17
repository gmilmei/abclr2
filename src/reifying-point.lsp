;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; reifying point object
;;; Dec. 14, 1991 by MASUHAR, H.

(in-package "ABCLR2-SYSTEM")

(defun reifying-point (denoted-object M R S)
  (send-message (meta-of denoted-object)
    (list :message M R S) nil denoted-object))

(defun state-list (obj)
  (cond ((null (object-data-meta obj)) nil)
        ((primary-meta-local-p
          (object-data-local (meta-of obj)))
         (car (object-data-local
               (primary-meta-local-state
                (object-data-local (meta-of obj))))))
        (t
         (let ((env (cdr (assoc 'state (state-list (meta-of obj))))))
             (if (and (object-data-p env)
                      (listp (object-data-local env)))
               (car (object-data-local env)))))))

(defun create-reifying-point-of (meta-obj)
  (make-object-data
   :port #'reifying-point
   :meta meta-obj
   :gid (let ((alist (state-list meta-obj)))
          (if (listp alist) (cdr (assoc 'group alist))))
   :type 'Denoted
   :status 'no-schedule
   :status-lock nil))
