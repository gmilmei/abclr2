;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; *hand-compiled* script for primary metaobject
;;; Nov. 10, 1991 by MASUHRA, H.
;;; Jul. 25, 1992 --- script with from field.
;;; Aug.  8, 1992 --- minor bug fix; :past instead of :send-past
;;; Nov. 22, 1992 --- evaluator can be changed
;;; Feb. 24, 1993 --- changes aronud self-eval type expressions

(in-package "ABCLR2-SYSTEM")

(defvar *primary-metaobject-script*)

(let (code-message code-begin code-end code-reify code-state code-scripts
      code-find-script code-add-script code-remove-script code-queue
      code-enqueue code-dequeue code-evaluator code-set-evaluator) 
  
  ;; {=> [:message Message Reply Sender]
  ;; L1: [queue <= [:enq [Message Reply Sender]]]
  ;; L2: {when (eq mode ':dormant)
  ;; L3:   [mode := ':active]
  ;; L4:   [Me <= :begin]}
  (labels
    ((code-L1 (C Env Id Gid Eval)
              (send-message Eval
                `(:do-evlis ((:variable queue) (:variable Message) 
                             (:variable Reply) (:variable Sender))
                            ,Env ,Id ,Gid ,Eval)
                (cont (list queue Message Reply Sender)
                  #+:ABCLR2-DEBUG (debug-print "message ~S for object~S~%"
                                               Message (den-of Id))
                  (send-message Eval
                    `(:do (:send-past ,queue
			   (:enq (,Message ,Reply ,Sender)) nil)
                          ,Env ,Id ,Gid ,Eval)
                    C Eval))
                Eval))
     ;;
     (code-L2 (C Env Id Gid Eval)
              (send-message Eval
                `(:do (:variable mode) ,Env ,Id ,Gid ,Eval)
                (cont mode
                  #+:ABCLR2-DEBUG (debug-print "mode=~S~%" mode)
                  (if (eq mode :dormant)
                    (send-message Eval
                      `(:do-progn ((:compiled ,#'code-L3) (:compiled ,#'code-L4))
                                  ,Env ,Id ,Gid ,Eval)
                      C Id)
                    (send-message C NIL nil Id)))
                Eval))
     ;;
     (code-L3 (C Env Id Gid Eval)
              (declare (ignore Gid Id))
              #+:ABCLR2-DEBUG
              (debug-print "setting the mode to :active.~%")
              (send-message Env '(:set mode :active) C Eval))
     ;;
     (code-L4 (C Env Id Gid Eval)
              (send-message Eval
                `(:do (:variable Me) ,Env ,Id ,Gid ,Eval)
                (cont Me
                  (send-message Eval
                    `(:do (:send-past ,Me :begin nil) ,Env ,Id ,Gid ,Eval)
                    C Eval))
                Eval)))
    
    (setf code-message
          `((:compiled ,#'code-L1) (:compiled ,#'code-L2))))
  
  ;; (=> :begin
  ;; L1: (match [queue <= :deq]
  ;;       (is [Message Reply Sender]
  ;; L3:       (match (find-script Message Reply Sender scriptset)
  ;; L4:         (is [Bindings ScriptBody]
  ;; L5:             [evaluator <=
  ;;                   [:do-progn
  ;;                       ScriptBody [env-gen <== [:new Bindings state]]
  ;;                       Me Group] @
  ;;                   [cont _
  ;;                     [Me <= :end]]])
  ;; L6:          (otherwise
  ;;                (warn "~S cannot handle the message ~S"
  ;;                      [den Me] Message)))))
  (labels
    ((code-L1 (C Env Id Gid Eval)
              (send-message Eval
                `(:do (:compiled ,#'code-L1-arg) ,Env ,Id ,Gid ,Eval)
                (cont MRS
                  #+:ABCLR2-DEBUG (debug-print "picked up a messgae ~S~%" arg)
                  (send-message Eval
                    `(:do (:compiled ,#'code-L3)
                          ,(extend-env (pairlis '(Message Reply Sender) MRS) Env)
                          ,Id ,Gid ,Eval)
                    C Eval))
                Eval))
     ;;
     (code-L1-arg (C Env Id Gid Eval)
                  ;; lookup queue
                  (send-message Eval
                    `(:do (:variable queue) ,Env ,Id ,Gid ,Eval)
                    (cont queue
                      ;; send :deq to queue
                      (send-message Eval
                        `(:do (:send-now ,queue :deq) ,Env ,Id ,Gid ,Eval)
                        C Eval))
                    Eval))
     ;;
     (code-L3 (C Env Id Gid Eval)
              ;; lookup Message,Reply, and scriptset
              (send-message Eval
                `(:do-evlis ((:variable Message) (:variable Reply)
			     (:variable Sender)
                             (:variable scriptset))
                            ,Env ,Id ,Gid ,Eval)
                (cont (list Message Reply Sender scriptset)
                  ;; call find-script
                  (let ((Bind&Script (find-script Message Reply Sender
						  scriptset)))
                    (cond
                     ((and (consp Bind&Script)
                           (= 2 (length Bind&Script)))
                      ;; when script was found ...
                      #+:ABCLR2-DEBUG
                      (debug-print "found a script ~S~%" ScriptBody)
                      (send-message Eval
                        `(:do (:compiled ,#'code-L5)
                              ,(extend-env (pairlis '(Bindings ScriptBody) Bind&Script)
                                           Env)
                              ,Id ,Gid ,Eval)
                        C Id))
                     ;; if script was not found ...
                     (t
                      (send-message Eval
                        `(:do (:compiled ,#'code-L6) ,Env ,Id ,Gid ,Eval)
                        C Id)))))
                Eval))
     ;;
     ;; send to 'evaluator' with evaluated argument
     (code-L5 (C Env Id Gid Eval)
              (send-message Eval
                `(:do-evlis ((:variable evaluator)
                             (:variable ScriptBody)
                             (:compiled ,#'code-l5-make-env)
                             (:variable Me)
                             (:variable Group)
                             (:compiled ,#'code-L5-make-reply))
                            ,Env ,Id ,Gid ,Eval)
                (cont (list evaluator ScriptBody new-env Me* GMgr reply-dest)
                  #+:ABCLR2-DEBUG
                  (debug-print "Sending 'start execution please' message~%")
                  (send-message Eval
                    `(:do (:send-past ,evaluator
                                      (:do ,ScriptBody
					   ,new-env ,Me* ,GMgr ,evaluator)
                                      ,reply-dest)
                          ,Env ,Id ,Gid ,Eval)
                    C Eval))
                Eval))
     ;;
     ;; argument evaluation [env-gen <== [:new Bindings state]]
     (code-L5-make-env (C Env Id Gid Eval)
                       (send-message Eval
                         `(:do-evlis ((:variable env-gen) (:variable Bindings)
                                      (:variable state)) ,Env ,Id ,Gid ,Eval)
                         (cont (list env-gen Bindings state)
                           (send-message Eval
                             `(:do (:send-now ,env-gen (:new ,Bindings ,state))
                                   ,Env ,Id ,Gid ,Eval)
                             C Id))
                         Id))
     ;;
     ;; evaluate the creation of reply destination
     (code-L5-make-reply (C Env Id Gid Eval)
       (send-message Eval
	 `(:do (:variable Me) ,Env ,Id ,Gid ,Eval)
	 (cont Me
	   (send-message Eval
	     `(:do (:object-def nil	; name
		    nil			; meta-gen-spec
		    nil			; group
		    ((Me* . (:self-evaluate ,#'(lambda () Me))))
		    (,(make-script
		       _
		       nil
		       nil
		       (list (list :compiled #'code-for-cont)))))
	       ,Env ,Id ,Gid ,Eval)
	     C Eval))
	 Eval))
     ;;
     (code-for-cont (C Env Id Gid Eval)
                    (send-message Eval
                      `(:do (:variable Me*) ,Env ,Id ,Gid ,Eval)
                      (cont Me*
                        #+:ABCLR2-DEBUG (debug-print "Sending :end to ~S~%"
                                                     Me*)
                        (send-message Eval
                          `(:do (:send-past ,Me* :end nil) ,Env ,Id ,Gid ,Eval)
                          C Id))
                      Eval))
     ;;
     (code-L6 (C Env Id Gid Eval)
              (send-message Eval
                `(:do-evlis ((:variable Me) (:variable Message))
                            ,Env ,Id ,Gid ,Eval)
                (cont (list Me Message)
                  (warn "~S cannot handle the message ~S"
                        (den-of Me) Message)
                  (send-message C nil nil Eval))
                Eval)))
    ;;
    (setf code-begin `((:compiled ,#'code-L1))))
  
  ;;   (=> :end
  ;;L1:   (if (not [queue <== :empty?])
  ;;L2:     [Me <= :begin]
  ;;L3:     [mode := 'dormant]))
  (labels
    ((code-L1 (C Env Id Gid Eval)
              #+:ABCLR2-DEBUG (debug-print "End of script execution...~%")
              (send-message Eval
                `(:do (:compiled ,#'code-L1-arg)
                      ,Env ,Id ,Gid ,Eval)
                (cont arg
                  (send-message Eval
                    `(:do (:compiled ,(if (not arg)
                                        #'code-l2
                                        #'code-L3))
                          ,Env ,Id ,Gid ,Eval)
                    C Id))
                Id))
     ;;
     (code-L1-arg (C Env Id Gid Eval)
                  #+:ABCLR2-DEBUG (debug-print "asking queue is empty~%")
                  (send-message Eval
                    `(:do (:variable queue) ,Env ,Id ,Gid ,Eval)
                    (cont queue
                      (send-message Eval
                        `(:do (:send-now ,queue :empty?)
                              ,Env ,Id ,Gid ,Eval)
                        C Eval))
                    Eval))
     ;;
     (code-L2 (C Env Id Gid Eval)
              #+:ABCLR2-DEBUG (debug-print "the queue is not empty~%")
              (send-message Eval
                (list :do (list :variable 'Me) Env Id Gid Eval)
                (cont Me
                  (send-message Eval
                    `(:do (:send-past ,Me :begin nil)
                          ,Env ,Id ,Gid ,Eval)
                    C Id))
                Eval))
     ;;
     (code-L3 (C Env Id Gid Eval)
              (declare (ignore Id Gid))
              #+:ABCLR2-DEBUG (debug-print "the queue is empty~%")
              (send-message Env '(:set mode :dormant) C Eval)))
    ;;
    (setf code-end `((:compiled ,#'code-L1))))
  
  (setf code-reify
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do '(:variable R) Env Id Gid Eval)
                          (cont R 
                            (send-message Eval
                              (list :do `(:send-past ,R t nil) Env Id Gid Eval)
                              C Eval))
                          Eval)))))
  
  (setf code-state
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable R) (:variable state))
                                Env Id Gid Eval)
                          (cont (list R state)
                            (send-message Eval
                              (list :do `(:send-past ,R ,state nil) Env Id Gid Eval)
                              C Eval))
                          Eval)))))

  (setf code-scripts
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable R) 
                                            (:variable scriptSet))
                                Env Id Gid Eval)
                          (cont (list R scriptSet)
                            (send-message Eval
                              (list :do
                                    `(:send-past ,R ,scriptSet nil)
                                    Env Id Gid Eval)
                              C Eval))
                          Eval)))))
  
  (setf code-find-script
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable R) (:variable Message)
                                            (:variable scriptSet))
                                Env Id Gid Eval)
                          (cont (list R Message scriptSet)
                            (send-message Eval
                              (list :do 
                                    `(:send-past ,R ,(search-script Message
						 nil nil scriptSet) nil)
                                    Env Id Gid Eval)
                              C Eval))
                          Eval)))))
  (setf code-add-script
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable new-script)
                                            (:variable scriptSet))
                                Env Id Gid Eval)
                          (cont (list new-script  scriptSet)
                            (send-message Env
                              (list :set 'scriptSet
				    (cons new-script scriptSet))
                              C Eval))
                          Eval)))))
  (setf code-remove-script
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable Message)
                                            (:variable scriptSet))
                                Env Id Gid Eval)
                          (cont (list Message scriptSet)
                            (send-message Env
                              (list :set 'scriptSet
				    (remove-script Message nil nil scriptSet))
                              C Eval))
                          Eval)))))
  
  (setf code-queue
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable queue)
                                            (:variable R))
                                Env Id Gid Eval)
                          (cont (list queue R)
                            (send-message Eval
                              (list :do `(:send-past ,queue :listify ,R)
                                    Env Id Gid Eval)
                              C Eval))
                          Eval)))))
  (setf code-enqueue
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable queue)
                                            (:variable item))
                                Env Id Gid Eval)
                          (cont (list queue item)
                            (send-message Eval
                              (list :do `(:send-past ,queue (:enq ,item) nil)
                                    Env Id Gid Eval)
                              C Eval))
                          Eval)))))
  (setf code-dequeue
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do-evlis '((:variable queue)
                                            (:variable R))
                                Env Id Gid Eval)
                          (cont (list queue R)
                            (send-message Eval
                              (list :do `(:send-past ,queue :deq R)
                                    Env Id Gid Eval)
                              C Eval))
                          Eval)))))
  (setf code-evaluator
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          (list :do '(:variable evaluator)
                                Env Id Gid Eval)
                          (cont evaluator 
                            (send-message Eval
                              (list :do '(:variable R) Env Id Gid Eval)
                              (cont R
                                (send-message Eval
                                  (list :do `(:send-past ,R ,evaluator nil)
                                        Env Id Gid Eval)
                                  C Eval)) Eval))
                          Eval)))))
  (setf code-set-evaluator
        (list (list :compiled
                    #'(lambda (C Env Id Gid Eval)
                        (send-message Eval
                          `(:do (:variable e) ,Env ,Id ,Gid ,Eval)
                          (cont e
                            (send-message Env
                              `(:set evaluator ,e) C Eval))
                          Eval)))))
  (setf *primary-metaobject-script*
        (list (make-script (list :message Message Reply Sender)
                           nil nil code-message)
              (make-script :begin
                           nil nil code-begin)
              (make-script :end
                           nil nil code-end)
              (make-script :reify-yourself
                           'R nil code-reify)
              (make-script :state
                           'R nil code-state)
              (make-script :scripts
                           'R nil code-scripts)
              (make-script (list :find-script Message)
                           'R nil code-find-script)
              (make-script (list :add-script Message)
                           'R nil code-add-script)
              (make-script (list :remove-script Message)
                           nil nil code-remove-script)
              (make-script :queue
                           'R nil code-queue)
              (make-script (list :enqueue item)
                           nil nil code-enqueue)
              (make-script :dequeue
                           'R nil code-dequeue)
              (make-script :evaluator
                           'R nil code-evaluator)
              (make-script (list :set-evaluator e)
                           nil nil code-set-evaluator)
              ))
  )