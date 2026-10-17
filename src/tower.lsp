;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; up and down reflective tower
;;; Nov. 3, 1991 MASUHARA, H.
;;; Jul.22, 1992 Error checking codes are inserted for non-object arguments.

(in-package "ABCLR2-SYSTEM")

;; (meta-of obj)
;; (den-of obj)
;; (group-of obj)

;; This function returns a metaobject of an object.
;; If the given object does not have its metaobject,
;; this function creates a Light-Weight-Meta.
(defun meta-of (object)
  (unless (object-data-p object)
    (error "Cannot take meta of non-object stuff ~S."
	   object))
  (or
   ;; if the object has metaobject, return it
   (object-data-meta object)
   ;; otherwise, create Light-Weight-Meta for the object.
   (let ((light-weight-meta
          (create-light-weight-meta-of object)))
     (setf (object-data-meta object) light-weight-meta)
     light-weight-meta)))

;; This function returns a denoting object of the given
;; object. If the object has no denoting object, a warning
;; is displayed.
(defun den-of (object)
  (unless (object-data-p object)
    (error "Cannot take denotation of non-object stuff ~S."
	   object))
  (or 
   ;; Metaobject and non-reifying-meta have their
   ;; denoting objects.
   (object-data-den object)
   ;; As user-defined objects can be used as metaobjects,
   ;; the access to "den" slot of a denoted object creates
   ;; a new reifying point.
   (let ((den-obj (create-reifying-point-of object)))
     (setf (object-data-den object) den-obj))
   ;; "den" slot of other objects is always nil.
   (warn "Cannot take den of ~S." object)
   (break)))

;; This function returns a group manager of the given object.
;; If it has no group manager (and it is primary group manager,)
;; a new group manager will be created.
(defun group-of (object)
  ;;(unless (object-data-p object)
  ;; (error "Cannot take group of non-object stuff ~S."
  ;;        object))
  (if (and object (object-data-p object))
    (or
     ;; All objects other than group manager have its group manager
     ;; in its metalevel.
     (object-data-gid object)
     ;; Group manager may have no group manager of itself.
     (if (eq 'GMgr (object-data-type object))
       (let ((GMgr (create-primary-GMgr)))
         (setf (object-data-gid object) GMgr)
         GMgr)
       ;; Taking GID of non-group manager object which has no 
       ;; group manager causes error or warning.
       #+COMMENT (error "This object ~A has no GID!" object)
       ))))

