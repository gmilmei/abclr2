;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; low-level message transportation
;;; Nov. 3, 1991 by MASUHARA, H.
;;; Jul.22, 1992 --- Error checking code for non-object target is added.

(in-package "ABCLR2-SYSTEM")

(defun lambda-closure-p (port-object)
  (and (not (symbolp port-object))
       (functionp port-object)))

;; When target object is null, no action is taken.
;; If the port of target object is lambda-closure,
;; sender's activity of execution is used for execution of
;; receiver's script.
(defun send-message (target message reply sender)
  (cond ((null target))			;do nothing
	((object-data-p target)
	 (let ((port (object-data-port target)))
	   (cond ((queue-p port)	; queue with semaphore
		  (enqueue port (list message reply sender))
		  (activate-object target))
		 ((lambda-closure-p port) ; lambda closure
		  ;; lend the activity of execution
		  (funcall port target message reply sender))
		 (t (abclr2-error "Target ~A for message send is not object." 
				  target message sender)))))
	(t				; non object structure
	 (error "Cannot send message to non-object type data.~%~
		 Target=~A Message=~A Reply=~A Sender=~A"
		target message reply sender))
    ))

;; dequeue-and-bind is supporting macro for metaobjects' descriptions.
;; (dequeue-and-bind port (Message Reply Sender) <forms>)
;; <forms> are executed  with Message, Reply, and Sender are bound 
;; to first, second, and third element of the dequeued message
;; packet.
(defmacro dequeue-and-bind (port variables &rest form)
  (unless (= 3 (length variables))
    (warn "Number of variables should be 3 for dequeue-and-bind,~
           but ~A are supplied.~%" (length variables)))
  (let ((MRS (gensym)))
    `(let ((,MRS (dequeue ,port)))
       #-KCLM(if (null ,MRS) (return))
       (let ((,(first variables) (first ,MRS))
             (,(second variables) (second ,MRS))
             (,(third variables) (third ,MRS)))
         ,@form))))
               
;; (primary-object-loop
;;   object
;;   ( { (variable-name acess-form [:write-back]) }* )
;;   port-name
;;   (message reply sender)
;;   {form}*)
;; This form gives a loop for primary objects. 
;; For example, a form:
;;   (primary-object-loop
;;     Me
;;     ((port (object-data-port Me))
;;      (local-data (object-data-local Me))
;;      (var-1 (car local))
;;      (var-2 (cdr local)))
;;     port (M R S)
;;     (match M ...) )
;;  is expnaded as follows:
;;  (let* ((port (object-data-port Me))
;;         (local-data (object-data-local Me))
;;         (var-1 (car local-data))
;;         (var-2 (cdr local-data)))
;;    (let ((*return-value* (loop 
;;                            (dequeue-and-bind
;;                             port (M R S)
;;                             (match M ...))
;;                            (tail-of-loop Me))))
;;      (setf (cdr local-data) var-2)
;;      (setf (car local-data) var-1)
;;      (setf (object-data-local Me) local-data)
;;      (setf (object-data-port Me) port)))

(defun bind-cache (binding-forms)
  (mapcar #'(lambda (binding-form)
              (cond ((= (length binding-form) 2) binding-form)
                    ((and (= (length binding-form) 3)
                          (eq (third binding-form)  ':write-back))
                     (butlast binding-form))
                    (t (error "Bad binding form for primary-object-loop~
                               ~A" binding-form))))
          binding-forms))

(defun restore-cache (binding-forms)
  (mapcar #'(lambda (binding-form)
              `(setf ,(second binding-form)
                     ,(first binding-form)))
          (remove-if-not
           #'(lambda (binding-form)
               (and (= (length binding-form) 3)
                    (eq (third binding-form) :write-back)))
           binding-forms)))
                     
(defun bind-to-restore (binding-form)
  `(setf ,(second binding-form)
         ,(first binding-form)))
        
(defmacro primary-object-loop
          (object-name cache-variables port-name message-bind 
                       &rest forms &aux (*return-value-holder*
                                         (gensym)))
  `(let* ,(bind-cache cache-variables)
     (let ((,*return-value-holder*
            (loop
              (dequeue-and-bind
               ,port-name ,message-bind
               ,@forms)
              (if (switch-context-p ,object-name)
		  (return))
		  )))
       ,@(restore-cache cache-variables)
       ,*return-value-holder*)))

(defun NRM-cache-binding-form (alist-sym variables)
  (mapcar #'(lambda (var) `(setq ,var (cdr (pop ,alist-sym))))
          variables))
(defun NRM-cache-restore-form (alist-sym variables)
  (mapcar #'(lambda (var) `(setf (cdr (pop ,alist-sym)) ,var))
          variables))

(defmacro primary-NRM-loop
    (object-name env-obj cache-variables message-bind
                 &rest forms
                 &aux (return-value-holder (gensym))
                      (port (gensym)) (local-data (gensym))
                      (bind-tmp (gensym))
                 )
  `(let ((,port (object-data-port ,object-name))
         (,local-data (object-data-local ,object-name))
         ,env-obj ,bind-tmp ,@cache-variables)
     (setq ,env-obj (cdr ,local-data)
           ,bind-tmp (car ,local-data))
     ,@(NRM-cache-binding-form bind-tmp cache-variables)
     (let ((,return-value-holder
            (loop
              (dequeue-and-bind
               ,port ,message-bind
               ,@forms)
              (if (switch-context-p ,object-name)
		  (return))
		  )))
       (setq ,bind-tmp (car ,local-data))
       ,@(NRM-cache-restore-form bind-tmp cache-variables)
       ,return-value-holder)))
					
