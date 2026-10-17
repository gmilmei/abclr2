;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; Lisp definition of primary metaobject
;;; Nov.10, 1991 by MASUHARA, H.
;;; Jul.25, 1992 --- script with from field.
;;; Jul.26, 1992 --- script :scripts is added.
;;; Sep.26, 1992 --- speed up by eliminating :begin message
;;; Nov.22, 1992 --- evaluator can be changed

(in-package "ABCLR2-SYSTEM")

(defvar use-new-primary-meta t
  "A switch whether to use the new, faster primary metaobjects.")

;;
;; (create-primary-meta Env Scripts Evaluator GMgr meta-Group)
;; Create primary metaobject and its process.
;;

(defun create-primary-meta (Scripts Evaluator GMgr meta-Group)
  (if use-new-primary-meta
      (new-create-primary-meta Scripts Evaluator GMgr meta-Group)
      (old-create-primary-meta Scripts Evaluator GMgr meta-Group)))

(defstruct primary-meta-local
  queue state scriptset evaluator Group mode
  send-me-end ; pre-defined continuation
  begin-proc
  )

(defun primary-meta-loop (Me)
  (if use-new-primary-meta
      (new-primary-meta-loop Me)
      (old-primary-meta-loop Me)))

(defun invoke-metaobject (metaObject)
  (if use-new-primary-meta
      (create-process #'new-primary-meta-loop metaObject)
      (create-process #'old-primary-meta-loop metaObject)))

(defun set-metaobject-state (metaobject state)
  (setf (primary-meta-local-state
	 (object-data-local metaobject)) state))

;;
;; new version
;;

(defun primary-meta-begin-proc-gen
    (Me evaluator Group scriptset send-me-end)
  #'(lambda (Message Reply Sender)
      (let ((state (primary-meta-local-state (object-data-local Me)))
	    (bind&script (find-script Message Reply Sender scriptset)))
	(if bind&script
	    (send-message evaluator
			  `(:do ,(second bind&script)
			    ,(if (first bind&script)
				 (extend-env (first bind&script) state)
				 state)
			    ,Me;; [den Me]!?
			    ,Group ,evaluator)
			  send-me-end
			  Me)
	    (warn "~S cannot handle the message ~S"
		  (den-of Me) Message)))))

(defun primary-meta-send-me-end-gen (Me)
  (let* ((local (object-data-local Me))
	 (queue (primary-meta-local-queue local))
	 (Evaluator (primary-meta-local-evaluator local)))
    (cont _
	  (if (and (not (emptyp queue))
		   (eq ':active (primary-meta-local-mode local)))
	      (let ((MRS (dequeue queue)))
		(funcall (primary-meta-local-begin-proc local)
			 (first MRS) (second MRS) (third MRS)))
	      (send-message Me :end nil Evaluator)))))

;; Script-list updates must refresh both the loop's cached function and
;; the slot used by the completion continuation. Do not restart evaluation
;; or change the queue, mode, state, or completion destination.
(defun primary-meta-refresh-begin-proc (Me evaluator scriptset)
  (let ((local (object-data-local Me)))
    (setf (primary-meta-local-begin-proc local)
          (primary-meta-begin-proc-gen
           Me evaluator (primary-meta-local-group local) scriptset
           (primary-meta-local-send-me-end local)))))

(defun new-primary-meta-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me))
       (metaState (object-data-local Me))
       (queue (primary-meta-local-queue metaState))
       (state (primary-meta-local-state metaState))
       (scriptset (primary-meta-local-scriptset metaState) :write-back)
       (evaluator (primary-meta-local-evaluator metaState))
       ;; (Group (primary-meta-local-Group metaState))
       (mode (primary-meta-local-mode metaState) :write-back)
       ;; (send-me-end (primary-meta-local-send-me-end metaState))
       (primary-meta-begin (primary-meta-local-begin-proc metaState)))
   port (M R S)
   (declare (ignore S))
   ;;
   #+:ABCLR2-DEBUG (debug-print "~S: M=~S R=~S S=~S.~%" Me M R S)
   (case M
     ;;
     ;; :end
     ;;
     (:end
      (if (not (emptyp queue))
	  (let ((MRS (dequeue queue)))
	    (funcall primary-meta-begin
		     (first MRS) (second MRS) (third MRS)))
	  (setf mode ':dormant)))
     ;;
     ;; :reify-yourself
     ;;
     (:reify-yourself
      (setf mode ':reified)
      (new-reify-metaobject Me (meta-of Me))
      ;; Now, Me is Denoted-object. 
      (send-message R t nil Me) ; Notification of the completion
      ;;(kill-process-for Me) ; The porcess for myself is no longer to use.
      )
     ;;
     ;; :state
     ;;   As the state is implemented by the environment object,
     ;;   it can replied directly.
     (:state
      (send-message R state nil Me))
     ;;
     ;; :queue
     ;;   Return a copy of the entries in the queue.
     (:queue
      (send-message R (queue-listify queue) nil Me))
     ;;
     ;; :dequeue
     ;;   Dequeue and return the first entry in the queue.
     (:dequeue
      (send-message R (dequeue queue) nil Me))
     ;;
     ;; :scripts
     ;;   List of all scripts are returned.
     (:scripts
      (send-message R scriptSet nil Me))
     ;;
     ;; :evaluator
     ;;   The evaluator is returned.
     (:evaluator
      (send-message R evaluator nil Me))
     ;;
     ;; :begin
     ;;   This is for compatibility
     (:begin
      (when (not (emptyp queue))
        (setf mode ':active)
        (match (dequeue queue)
          (is (list Message Reply Sender)
              (funcall primary-meta-begin Message Reply Sender)))))
     ;;
     ;; complex messages
     ;;
     (t
      (if (consp M)
        (case (first M)
          ;;
          ;; [:message Message Reply Sender]
          ;;
          (:message
           (if (eq mode ':dormant)
	       (progn
		 (setf mode ':active)
		 (funcall primary-meta-begin
			  (second M) (third M) (fourth M)))
	       (enqueue queue (rest M))))
          ;;
          ;; [:find-script Message]
          ;;
          (:find-script
           (send-message R
             (search-script (second M) nil nil scriptset)
             nil Me))
          ;;
          ;; [:add-script new-script]
          ;;
          (:add-script
           (push (second M) scriptset)
           (setf primary-meta-begin
                 (primary-meta-refresh-begin-proc Me evaluator scriptset)))
          ;;
          ;; [:remove-script Message]
          ;;
          (:remove-script
           (setf scriptSet (remove-script (second M) nil nil scriptSet)
                 primary-meta-begin
                 (primary-meta-refresh-begin-proc Me evaluator scriptset)))
          ;;
          ;; [:enqueue item]
          ;;
          (:enqueue
           (enqueue queue (second M)))
          ;;
          ;; [:set-evaluator e]
          ;;
          (:set-evaluator
           (setf evaluator (second M)
                 (primary-meta-local-evaluator metaState) (second M)
                 primary-meta-begin
                 (primary-meta-refresh-begin-proc Me evaluator scriptset)))
          ;;
          (t
           (warn "~S cannot handle the message ~S"
                 Me M)))
        (warn "~S cannot handle the message ~S"
                 Me M))))))

(defun new-create-primary-meta (Scripts Evaluator GMgr meta-Group)
  ;; the state will be given later
  (let* ((metaObj (make-object-data
		   :port (create-queue)
		   :meta nil
		   :gid meta-Group
		   :local (make-primary-meta-local
			   :queue (create-queue)
			   :scriptset Scripts
			   :evaluator Evaluator
			   :Group GMgr
			   :mode ':dormant)
		   :type 'Meta
		   :status 'dormant))
	 (denObj (make-object-data
		  :port #'reifying-point
		  :meta metaObj
		  :den nil
		  :gid GMgr
		  :type 'Denoted
		  :status 'no-schedule
		  :status-lock nil))
	 (send-me-end (primary-meta-send-me-end-gen metaObj)))
    (setf (object-data-den metaObj) denObj)
    (setf (primary-meta-local-send-me-end (object-data-local metaObj))
	  send-me-end)
    (setf (primary-meta-local-begin-proc (object-data-local metaObj))
	  (primary-meta-begin-proc-gen
	   metaObj Evaluator GMgr scripts send-me-end))
    metaObj))

(defun new-reify-metaobject (den-obj meta-obj)
  ;; data structure for new metaobject
  ;;   port: create a new queue with semaphore
  ;;   meta: unchanged
  ;;   den: unchanged
  ;;   gid: unchanged
  ;;   local...
  ;;      queue: use old meta-obj's port
  ;;      state: pack den-obj's metaState
  ;;      scriptset: use default script for metaobject
  ;;      evaluator: [[group-of den-obj] <== :evaluator]
  ;;      Group: [group-of meta-obj]
  ;;      mode: either :dormant or :active (according to queue)
  ;;      send-me-end: create newly
  ;;    type: Meta
  ;;    status: dormant
  ;;
  ;; data structure for old metaobject (den-obj)
  ;;   port: reifying-point
  ;;   meta,den,gid: unchanged
  ;;   local: nil
  ;;   type: Denoted
  ;;   status: no-schedule
  ;;   status-lock: nil
       (let*
         ((den-metaState ; metaState for den-obj
           (object-data-local den-obj))
          (eval-at-meta (send-present-message (group-of den-obj)
                                              :evaluator meta-obj))
          (meta-metaState ; metaState for meta-obj
           (make-primary-meta-local
            :queue (object-data-port den-obj)
            :state (extend-env
                    `((queue . ,(create-queue-obj 
                                 (group-of den-obj)
                                 (primary-meta-local-queue den-metaState)))
                      (state . ,(primary-meta-local-state den-metaState))
                      (scriptSet 
                       . ,(primary-meta-local-scriptSet den-metaState))
                      (evaluator 
                       . ,(primary-meta-local-evaluator den-metaState))
                      (Group . ,(primary-meta-local-Group den-metaState))
                      (mode . ,(primary-meta-local-mode den-metaState)))
                    *global-env*)
            :scriptSet *primary-metaobject-script*
            :evaluator eval-at-meta
            :Group (group-of den-obj)
            :mode ':dormant)))
         (setf (object-data-local meta-obj) meta-metaState)
         (let ((send-me-end (primary-meta-send-me-end-gen meta-obj)))
           (setf (primary-meta-local-send-me-end meta-metaState)
	         send-me-end
	         (primary-meta-local-begin-proc meta-metaState)
	         (primary-meta-begin-proc-gen
	          meta-obj eval-at-meta (group-of den-obj)
	          *primary-metaobject-script* send-me-end))
           (setf (object-data-port meta-obj) (create-queue)
                 (object-data-local meta-obj) meta-metaState
                 (object-data-type meta-obj) 'Meta
                 (object-data-status meta-obj) 'dormant
	         (object-data-status-lock meta-obj) #+KCLM (si:make-lock) #-KCLM nil)
           
           (create-process #'new-primary-meta-loop meta-obj)
           
           (setf (object-data-port den-obj) #'reifying-point
                 (object-data-local den-obj) nil
                 (object-data-type den-obj) 'Denoted
                 (object-data-status den-obj) 'no-schedule
                 ;; (object-data-status-lock den-obj) nil
	         ))))

;;
;; old version
;;
                             
(defun old-primary-meta-loop (Me)
  (primary-object-loop
   Me ((port (object-data-port Me))
       (metaState (object-data-local Me))
       (queue (primary-meta-local-queue metaState))
       (state (primary-meta-local-state metaState))
       (scriptset (primary-meta-local-scriptset metaState) :write-back)
       (evaluator (primary-meta-local-evaluator metaState))
       (Group (primary-meta-local-Group metaState))
       (mode (primary-meta-local-mode metaState) :write-back)
       (send-me-end (primary-meta-local-send-me-end metaState)))
   port (M R S)
   (declare (ignore S))
   ;;
   #+:ABCLR2-DEBUG (debug-print "~S: M=~S R=~S S=~S.~%" Me M R S)
   (case M
     ;;
     ;; :begin
     ;;
     (:begin
      (let* (;; [Message Reply Sender]
	     (MRS  (dequeue queue))
	     ;; [Bindings ScriptBody]
	     (bind&script (find-script (first MRS) (second MRS) (third MRS)
				       scriptset)))
	(if bind&script
	    (send-message evaluator
			  `(:do ,(second bind&script)
			    ,(if (first bind&script)
				 (extend-env (first bind&script) state)
				 state)
			    ,Me;; [den Me]!?
			    ,Group ,evaluator)
			  send-me-end
			  Me)
	    (warn "~S cannot handle the message ~S"
		  (den-of Me) (first MRS)))))
     ;;
     ;; :end
     ;;
     (:end
      (if (not (emptyp queue))
	  (send-message Me :begin nil Me)
	  (setf mode ':dormant)))
     ;;
     ;; :reify-yourself
     ;;
     (:reify-yourself
      (old-reify-metaobject Me (meta-of Me))
      ;; Now, Me is Denoted-object. 
      (send-message R t nil Me) ; Notification of the completion
      ;;(kill-process-for Me) ; The porcess for myself is no longer to use.
      )
     ;;
     ;; :state
     ;;   As the state is implemented by the environment object,
     ;;   it can replied directly.
     (:state
      (send-message R state nil Me))
     ;;
     ;; :queue
     ;;   Return a copy of the entries in the queue.
     (:queue
      (send-message R (queue-listify queue) nil Me))
     ;;
     ;; :dequeue
     ;;   Dequeue and return the first entry in the queue.
     (:dequeue
      (send-message R (dequeue queue) nil Me))
     ;;
     ;; :scripts
     ;;   List of all scripts are returned.
     (:scripts
      (send-message R scriptSet nil Me))
     ;;
     ;; complex messages
     ;;
     (t
      (if (consp M)
        (case (first M)
          ;;
          ;; [:message Message Reply Sender]
          ;;
          (:message
           (enqueue queue (rest M))
           (when (eq mode ':dormant)
             (setf mode ':active)
             (send-message Me :begin nil Me)))
          ;;
          ;; [:find-script Message]
          ;;
          (:find-script
           (send-message R
             (search-script (second M) nil nil scriptset)
             nil Me))
          ;;
          ;; [:add-script new-script]
          ;;
          (:add-script
           (push (second M) scriptset))
          ;;
          ;; [:remove-script Message]
          ;;
          (:remove-script
           (setf scriptSet (remove-script (second M) nil nil scriptSet)))
          ;;
          ;; [:enqueue item]
          ;;
          (:enqueue
           (enqueue queue (second M)))
          ;;
          (t
           (warn "~S cannot handle the message ~S"
                 Me M)))
        (warn "~S cannot handle the message ~S"
                 Me M))))))

(defun old-create-primary-meta (Scripts Evaluator GMgr meta-Group)
  ;; the state will be given later
  (let* ((metaObj (make-object-data
		   :port (create-queue)
		   :meta nil
		   :gid meta-Group
		   :local (make-primary-meta-local
			   :queue (create-queue)
			   :scriptset Scripts
			   :evaluator Evaluator
			   :Group GMgr
			   :mode ':dormant)
		   :type 'Meta
		   :status 'dormant))
	 (denObj (make-object-data
		  :port #'reifying-point
		  :meta metaObj
		  :den nil
		  :gid GMgr
		  :type 'Denoted
		  :status 'no-schedule
		  :status-lock nil)))
    (setf (object-data-den metaObj) denObj)
    (setf (primary-meta-local-send-me-end (object-data-local metaObj))
          (cont _ (send-message metaObj :end nil metaObj)))
    metaObj))

(defun old-reify-metaobject (den-obj meta-obj)
  ;; data structure for new metaobject
  ;;   port: create a new queue with semaphore
  ;;   meta: unchanged
  ;;   den: unchanged
  ;;   gid: unchanged
  ;;   local...
  ;;      queue: use old meta-obj's port
  ;;      state: pack den-obj's metaState
  ;;      scriptset: use default script for metaobject
  ;;      evaluator: [[group-of den-obj] <== :evaluator]
  ;;      Group: [group-of meta-obj]
  ;;      mode: either :dormant or :active (according to queue)
  ;;      send-me-end: create newly
  ;;    type: Meta
  ;;    status: dormant
  ;;
  ;; data structure for old metaobject (den-obj)
  ;;   port: reifying-point
  ;;   meta,den,gid: unchanged
  ;;   local: nil
  ;;   type: Denoted
  ;;   status: no-schedule
  ;;   status-lock: nil
  (let*
    ((den-metaState ; metaState for den-obj
      (object-data-local den-obj))
     (meta-metaState ; metaState for meta-obj
       (make-primary-meta-local
        :queue (object-data-port den-obj)
        :state (extend-env
                `((queue . ,(create-queue-obj 
                               (group-of den-obj)
                               (primary-meta-local-queue den-metaState)))
                    (state . ,(primary-meta-local-state den-metaState))
                    (scriptSet 
                     . ,(primary-meta-local-scriptSet den-metaState))
                    (evaluator 
                     . ,(primary-meta-local-evaluator den-metaState))
                    (Group . ,(primary-meta-local-Group den-metaState))
                    (mode . ,(primary-meta-local-mode den-metaState)))
                *global-env*)
        :scriptSet *primary-metaobject-script*
        :evaluator (send-present-message (group-of den-obj)
                     :evaluator meta-obj)
        :Group (group-of den-obj)
        :mode ':dormant
        :send-me-end (cont _ (send-message meta-obj :end nil meta-obj)))))
    
    (setf (object-data-port meta-obj) (create-queue)
          (object-data-local meta-obj) meta-metaState
          (object-data-type meta-obj) 'Meta
          (object-data-status meta-obj) 'dormant
	  (object-data-status-lock meta-obj) #+KCLM (si:make-lock) #-KCLM nil)
    
    (create-process #'old-primary-meta-loop meta-obj)
    
    (setf (object-data-port den-obj) #'reifying-point
          (object-data-local den-obj) nil
          (object-data-type den-obj) 'Denoted
          (object-data-status den-obj) 'no-schedule
          ;; (object-data-status-lock den-obj) nil
	  )))
	
