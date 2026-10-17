;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; common structure for object
;;; Nov. 3, 1991 by MASUHARA, H.
;;; Jul. 7, 1992 more straightforward printing function

(in-package "ABCLR2-SYSTEM")

;; structure for all objects
(defstruct (object-data (:print-function print-object-data))
  port		; message port
  meta		; pointer to metaobject
  den		; pointer to denoting object
  gid		; pointer to (meta) group manager
  local		; slot to store local infomation
  type		; {GMgr,Eval,Meta-Gen,Meta,Denoted,NRM,LW-meta,Cont}
  status	; {active, dormant, dead, no-schedule}
  ;; lock for status; creating non-scheduled object, nil should 
  ;; be specified.
  (status-lock (make-lock))
  pid		; process id
  name		; name of the object (if specified)
  )

#+COMMENT
(defun induce-object-name (object &key direction &aux neighbor-name)
  (cond ((object-data-name object))
        ((and (object-data-meta object)
              (not (eq direction :den))
              (setq neighbor-name
                    (induce-object-name (meta-of object)
                                        :direction :meta)))
         (setf (object-data-name object)
               (format nil "[den ~A]" neighbor-name)))
        ((and (object-data-den object)
              (not (eq direction :meta))
              (setq neighbor-name
                    (induce-object-name (den-of object)
                                        :direction :den)))
         (setf (object-data-name object)
               (format nil "[meta ~A]" neighbor-name)))
        (t nil)))

;;
;; Object printing function
;;
;; An object is named following rules:
;;  1. If the object is created by [object NAME ...], its name is
;;  #<NAME-xx> where xx is some integer number.
;;  2. If the object is metaobject, meta-metaobject, ..., of object
;;  #<FOO-n>, its name is #[meta [meta ... [meta #<FOO-n>]..]].
;;  3. If the object is denotation of an object named X, its name is
;;  #[den X].
;;  4. If the object 
;;
;; Implementation:
;;	The number of objects that are printed is much smaller than
;;	the one of objects that are cretaed. Therefore, objects are
;;	named in a lazy manner.
;;	Current implementation is:
;;	1. When an object named FOO is created, only symbol is
;;      assigned to it.
;;      2. When it is printed, it is given a string which is to be
;;      printed to the screen.
;;	3. Metaobjects and denotations are created with pointers to
;;	a named object. For example, if an object X named FOO and its
;;      metaobject are created, the metaobject has a pointer ('meta X).
;;      4. When a metaobject or a denotation is printed, its name is
;;      created according to the pointer. In the above example, the
;;      metaobject is to be named as #[meta #<FOO>].

;; Variable *object-naming-counter* is refered and incremented when an
;; unnamed object is to be printed.
(defvar *object-naming-counter* 0
  "Counter giving a unique number for objects printing.")

;; Print object in a apropriate manner.
;; If the object does not have naming information, the type of the
;; object (e.g. primary-meta) is used.
(defun print-object-data (object stream l)
  (declare (ignore l))
  (format stream (set&get-name object)))

(defun set&get-name (object)
  (let ((name (object-data-name object)))
    (cond ((null name);; no name
	   (let (generated-name)
	     (cond ((setq generated-name
			  (generate-name-pointer-from-den object))
		    ;; generate by denotation's name
		    (setf (object-data-name object) generated-name)
		    (set&get-name object))
		   ((setq generated-name
			  (generate-name-pointer-from-meta object))
		    ;; generate by metaobject's name
		    (setf (object-data-name object) generated-name)
		    (set&get-name object))
		   ((setq generated-name (object-data-type object))
		    ;; generate by object type
		    (setf (object-data-name object) generated-name)
		    (set&get-name object))
		   (t
		    ;; name as ``object''
		    (setf (object-data-name object) 'object)
		    (set&get-name object)))))
	  ((stringp name) name);; has name
	  ((symbolp name);; name is given, but not printed
	   (setf (object-data-name object)
		 (make-up-printing-by-symbol name)))
	  ((listp name);; someone in its tower has a name 
	   (setf (object-data-name object)
		 (make-up-printing-by-list name)))
	  (t (error "(fatal) object naming slot is corrupted: ~S."
		    name)))))

(defun make-up-printing-by-symbol (symbol)
  (format nil "#<~A-~D>" (string-downcase (symbol-name symbol))
		 (incf *object-naming-counter*)))

(defun make-up-printing-by-list (naming-list)
  (format nil (make-up-printing-string naming-list)))

(defun make-up-printing-string (object-or-list)
  (if (object-data-p object-or-list)
      (format nil "~A" object-or-list)
      (format nil "#[~A ~A]"
	      (string-downcase (symbol-name (first object-or-list)))
	      (make-up-printing-string (second object-or-list)))))

(defun generate-name-pointer-from-den (object)
  (let ((name (object-data-name object))
	(den (object-data-den object)))
    (cond (name object)			; named object
	  (den				; has denotation
	   (let ((den-pointer (generate-name-pointer-from-den den)))
	     (and den-pointer
		 (list 'meta den-pointer))))
	  (t nil))))			; both name&den are empty

(defun generate-name-pointer-from-meta (object)
  (let ((name (object-data-name object))
	(meta (object-data-meta object)))
    (cond (name object)			; named object
	  (meta				; has denotation
	   (let ((meta-pointer (generate-name-pointer-from-meta meta)))
	     (and meta-pointer
		 (list 'den meta-pointer))))
	  (t nil))))
