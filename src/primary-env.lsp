;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Primary environment generator
;;; Nov. 10, 1991 by MASUHARA, H.
;;; Jul. 12, 1992 acceptance of messages :lexEnv :alist is added

(in-package "ABCLR2-SYSTEM")

;; (extend-env new-Frame outer-env)
;; (create-global-env meta-group init-pairlis)

;;[object env-gen
;;  (=> [:new localFrame LexEnv]
;;      ![object env
;;        (script
;;         (=> [:value-of Name] @ R
;;          (let ((p (assoc Name LocalFrame :test #'eq)))
;;            (if p [R <= (cdr p)]
;;              [LexEnv <= [:value-of Name @ R]])))
;;         (=> [:set Name Value] @ R
;;          (let ((p (assoc Name LocalFrame :test #'eq)))
;;            (if p 
;;              (progn (setf (cdr p) Value)
;;                     [R <= Value])
;;              [LexEnv <= [:set Name Value] @ R])))
;;         (=> [:boundp Name] @ R
;;          (if (assoc Name LocalFrame :test #'eq)
;;            [R <= t]
;;            [LexEnv <= [:boundp Name] @ R])
;;         (=> :lexENv !LexEnv)
;;         (=> :alist !localFrame))])]

(defun env-port (env M R S)
  (declare (ignore S))
  ;;
  (if (consp M)
    (case (first M)
      ;;
      ;; [:value-of Name]
      ;;
      (:value-of
       (let ((p (assoc (second M) (car (object-data-local env)) :test #'eq)))
         (if p (send-message R (cdr p) nil env)
             (send-message (cdr (object-data-local env)) M R env))))
      ;;
      ;; [:set Name Value]
      ;; 
      (:set
       (let ((p (assoc (second M) (car (object-data-local env)) :test #'eq)))
         (if p (send-message R (setf (cdr p) (third M)) nil env)
             (send-message (cdr (object-data-local env)) M  R env))))
      ;;
      ;; [:set-local Name Value]
      ;;
      (:set-local
       (let ((p (assoc (second M) (car (object-data-local env)) :test #'eq)))
         (send-message R
                       (if p 
                         (setf (cdr p) (third M))
                         (progn (push (cons (second M) (third M))
                                      (car (object-data-local env)))
                                (third M)))
                       nil env)))
      ;;
      ;; [:boundp Name]
      ;; 
      (:boundp
       (if (assoc (second M) (car (object-data-local env)) :test #'eq)
         (send-message R t nil env)
         (send-message (cdr (object-data-local env)) M R env)))
      ;;
      (t (abclr2-warn "~S(env) cannot handle the message ~S"
                      env M)))
    (progn
      (case M
        ;;
        ;; :lexEnv
        ;;
        (:lexEnv
         (send-message R (cdr (object-data-local env)) nil env))
        ;;
        ;; :alist
        ;;
        (:alist
         (send-message R (car (object-data-local env)) nil env))
        (t
         (abclr2-warn "~S(env) cannot handle the message ~S"
                      env M))))
    ))

;; [object *global-env*
;;  (state [Frame := nil])
;;  (script
;;   (=> [:value-of Name]
;;       (temporary [p := (assoc Name Frame :test #'eq)])
;;       (if p 
;;         !(cdr p) 
;;         (error "~S is not bound" Name)))
;;   (=> [:set Name Value]
;;       (temporary [p := (assoc Name Frame :test #'eq)])
;;       !(if p (setf (cdr p) Value)
;;           (progn (warn "~S is bound to global" Name)
;;                  (push (cons Name Value) Frame)
;;                  Value)))
;;   (=> [:boundp Name]
;;       (temporary [p := (assoc Name Frame :test #'eq)])
;;       !(not (null p)))
;;   (=> :lexEnv !nil)
;;   (=> :alist !Frame))]
                 
(defun global-env-port (Me M R S)
  (declare (ignore S))
  (let ((Frame (object-data-local Me)))
    ;;
    (if (consp M)
      (ecase (first M)
        ;;
        ;; [:value-of Name]
        ;;
        (:value-of
         (let ((p (assoc (second M) Frame :test #'eq)))
           (if p (send-message R (cdr p) nil Me)
               (abclr2-error "~S is not bound" (second M))
               )))
        ;;
        ;; [{:set,:set-local} Name Value]
        ;;
        ((:set :set-local)
         (let ((p (assoc (second M) Frame :test #'eq)))
           (if p (send-message R (setf (cdr p) (third M)) nil Me)
               (progn
                 (abclr2-warn "~S is bound to global" (second M))
                 (push (cons (second M) (third M))
                       (object-data-local Me))
                 (send-message R (third M) nil Me))
               )))
        ;;
        ;; [{:multiple-set,:multiple-set-local} PairLis]
        ;;
        ((:multiple-set :multiple-set-local)
         (let* ((bounded (intersection (second M) Frame :key #'first))
                (new (set-difference (second M) Frame :key #'first)))
           (dolist (pair bounded)
             (setf (cdr (assoc (car pair) Frame))
                   (cdr pair)))
           (setf (object-data-local Me) 
                 (append new Frame))))
        ;;
        ;; [:boundp Name]
        ;;
        (:boundp
         (send-message R
                       (if (assoc (second M) Frame :test #'eq)
                         t nil)
                       nil Me)
         ))
      ;; M is symbol
      (ecase M
        ;;
        ;; :lexEnv
        ;;
        (:lexenv (send-message R nil nil Me))
        ;;
        ;; :alist
        ;;
        (:alist (send-message R Frame nil Me))))
    ))

(defun extend-env (localFrame LexEnv)
  (make-object-data
   :port #'env-port
   :meta nil 
   :den nil
   :gid (group-of LexEnv)
   :local (cons (if (eq localFrame t) nil localFrame)
                LexEnv)
   :type 'Cont
   :status 'no-schedule
   :status-lock nil
   :name 'state))

(defun env-gen-port (Me M R S)
  (declare (ignore S))
  ;;
  ;; [:new localFrame LexEnv]
  ;;
  (if (and (consp M) (eq :new (first M)))
    (send-message R (extend-env (second M) (third M)) nil Me)
    (abclr2-warn "~S cannot handle message ~S"
          Me M)))

(defun create-env-gen (meta-group)
  (make-object-data :port #'env-gen-port
		    :meta nil
		    :den nil
		    :gid meta-group
		    :local nil
		    :type 'Cont
		    :status 'no-schedule
		    :status-lock nil
                    :name 'env-gen))

(defun create-global-env (meta-group init-pairlis)
  (make-object-data :port #'global-env-port
		    :meta nil
		    :den nil
		    :gid meta-group
		    :local init-pairlis
		    :type 'Cont
		    :status 'no-schedule
		    :status-lock nil
                    :name 'global-env))
