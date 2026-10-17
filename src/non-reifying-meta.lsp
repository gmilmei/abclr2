;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; non reifying metaobject creator
;;; Dec. 10, 1991 by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

;; non-reifying-metaobject (NRM) creation form:
;; [:new stateInit lexEnv script dummy GMgr]

(defun NRM-gen-port (Me M R S)
   (declare (ignore S))
   ;;
   (assert (and (consp M)
		(eq (first M) ':new)
		(= (length M) 6)))
   ;;
   (let ((stateVarList (first (second M)))
	 (stateInitFunc (second (second M)))
	 (lexEnv (third M))
	 (scriptFunc (fourth M))
	 (GMgr (sixth M)))
     (let ((NRM (make-object-data :port (create-queue)
				  :gid (group-of GMgr)
				  :type 'NRM
				  :status 'dormant))
	   (den-NRM (make-object-data :port #'reifying-point
				      :gid GMgr
				      :type 'Denoted
				      :status 'no-schedule
				      :status-lock nil))
           (env-alist (mapcar #'(lambda (var) (cons var nil))
                              stateVarList) ))
       (setf (object-data-den NRM) den-NRM
	     (object-data-meta den-NRM) NRM
	     (object-data-local NRM)
             (cons env-alist (extend-env env-alist lexEnv)))
       ;; initialize state variables
       (funcall stateInitFunc env-alist NRM GMgr)
       ;; assign thread
       (create-process scriptFunc NRM)
       ;; returns created object
       (send-message R NRM nil Me))))

#+COMMENT
(defun NRM-gen-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me)))
   port (M R S)
   (declare (ignore S))
   ;;
   (assert (and (consp M)
		(eq (first M) ':new)
		(= (length M) 6)))
   ;;
   (let ((stateVarList (first (second M)))
	 (stateInitFunc (second (second M)))
	 (lexEnv (third M))
	 (scriptFunc (fourth M))
	 (GMgr (sixth M)))
     (let ((NRM (make-object-data :port (create-queue)
				  :gid (group-of GMgr)
				  :type 'NRM
				  :status 'dormant))
	   (den-NRM (make-object-data :port #'reifying-point
				      :gid GMgr
				      :type 'Denoted
				      :status 'no-schedule
				      :status-lock nil))
	   (env (extend-env (mapcar #'(lambda (var)
					(cons var nil))
				    stateVarList)
			    lexEnv)))
       (setf (object-data-den NRM) den-NRM
	     (object-data-meta den-NRM) NRM
	     (object-data-local NRM) env)
       ;; initialize state variables
       (funcall stateInitFunc env NRM GMgr)
       ;; assign thread
       (create-process scriptFunc NRM)
       ;; returns created object
       (send-message R NRM nil Me)))))

(defun create-NRM-gen (Group)
  (make-object-data :port #'NRM-gen-port
                           #+COMMENT(create-queue)
			   :gid Group
			   :type 'Cont
			   :status 'no-schedule
			   :name 'non-reifying-meta))

;;  (let ((NRM-gen
;;	 ))
;;    (create-process #'NRM-gen-loop NRM-gen)
;;    NRM-gen))
