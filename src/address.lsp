;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; unique address for each object --- implementation INdependent ---
;;; Nov. 3, 1991 by MASUHARA, H.
;;; Jul. 4, 1992 --- generic sturcutre with a conter is introduced.

(in-package "ABCLR2-SYSTEM")

;; 
(defvar *object-counter* 0
  "This counter is refered and incremented with each creation. It is
used by priting functions.")
(defvar *object-counter-lock* (make-lock))
(defstruct object
  "A data structure that has unique address. This is used for many
kinds of data structures that requires unique addresses for printing."
  (address (prog2
             (lock *object-counter-lock*)
             (incf *object-counter*)
             (unlock *object-counter-lock*))
             :type fixnum))

;; Structure instances are printed with their addresses for
;; identification. However, some Lisp implementations move objects for
;; GC, thus the address of an objects cannot be used for identification.
#|
#-(or KCL :CCL :CMU)
(defvar *object-address-counter* 0)
#-(or KCL :CCL :CMU)
(defvar *object-address-alist* nil)

(defun object-address (object)
  #+KCL (si:address object)
  #+:CCL #-:CCL-2 (ccl::%bin-address object)
         #+:CCL-2 (%address-of object)
  #+:CMU (kernel::get-lisp-obj-address object)
  #-(or KCL :CCL :CMU)
  (let ((id (assoc object *object-address-alist* :test #'eq)))
    (if id
      (cdr id)
      (let ((new-id *object-address-counter*))
        (push (cons object new-id)
              *object-address-alist*)
        (incf *object-address-counter*)
        new-id))))
|#