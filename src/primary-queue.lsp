;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; primary queue
;;; Nov. 12, 1991 by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

;; The queue object is implemented by low-queue.
;;
;;[object queue-gen
;; (=> :new
;;     ![object queue
;;       (state [q := (create-queue)])
;;       (script
;;         (=> :empty? !(emptyp q))
;;         (=> :deq @ R
;;              [[object queue-sub
;;                (script (=> :wait [R <= (dequeue q)]))] <= :wait])
;;	(=> [:enq item] (enqueue q item)))])]
;;
;; Notice that the queue object does not blocked even if :deq request
;; comes when the q is empty. On the other hand, the sender is blocked
;; until the corresponding message arrives.

(defun queue-port (queue-obj M R S)
  (declare (ignore S))
  (let ((queue (object-data-local queue-obj)))
    ;;
    (match M
      (is :empty?
          (send-message R (emptyp queue) nil queue-obj))
      (is :deq
          (let ((item (dequeue queue)))
            #+:ABCLR2-DEBUG (debug-print "dequeued item ~S ~%" item)
            (send-message R item nil queue-obj)))
      (is (list :enq item)
          #+:ABCLR2-DEBUG (debug-print "queuing item ~S~%" item)
          (enqueue queue item)
          (send-message R item nil queue-obj))
      (is :listify
          (send-message R (queue-listify queue) nil queue-obj))
      (otherwise
       (warn "Queue object ~S cannot handle message ~S"
             queue-obj M)))))

(defun create-queue-obj (group &optional q)
  (if (null q) (setq q (create-queue)))
  (make-object-data :port #'queue-port
		    :meta nil :den nil
		    :gid group
		    :local q
		    :type 'Cont
		    :status 'no-schedule
		    :status-lock nil
                    :name 'queue))

(defun queue-gen-port (queue-gen M R S)
  (declare (ignore S))
  ;;
  (match M
    (is :new
	(send-message R
	  (create-queue-obj (group-of queue-gen))
	  nil queue-gen))
    (otherwise
     (warn "queue-gen ~S cannot handle message ~S"
	   queue-gen M))))

(defun create-queue-gen (group)
  (make-object-data :port #'queue-gen-port
		    :meta nil :den nil
		    :gid group
		    :type 'Cont
		    :status 'no-schedule
                    :name 'queue-gen))

	  
		    