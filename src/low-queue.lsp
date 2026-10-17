;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; low-level definition of queue structure
;;; Nov. 3, 1991

(in-package "ABCLR2-SYSTEM")

;; (create-queue) creates a new queue with semaphore.
;; (dequeue q) picks up a message from q.
;; (enqueue q m) enqueues m into q.
;; (emptyp q) test q is empty.
;; (queue-listify q) returns list of all entries

(defstruct (queue 
            (:include object)
            (:print-function print-queue))
  (semaphore #+KCLM (make-semaphore)
             #-KCLM nil)
  (message-list nil)
  (mutex (make-lock)))

(defun print-queue (x s l)
  (declare (ignore l))
  (format s "#<queue(~A) ~X>"
          (if (emptyp x) "empty" "mesage")
          (object-address x)))

(defun create-queue ()
  (let ((q (make-queue)))
    (unlock (queue-mutex q))
    q))

(defun dequeue (queue)
  #+KCLM(semaphore-wait (queue-semaphore queue))
  (lock (queue-mutex queue))
  (prog1 (pop (queue-message-list queue))
    (unlock (queue-mutex queue))))

(defun enqueue (queue message)
  (lock (queue-mutex queue))
  (let ((p (list message)))
    (if (null (queue-message-list queue))
      (setf (queue-message-list queue) p)
      (nconc (queue-message-list queue) p))
    (unlock (queue-mutex queue))
    #+KCLM(semaphore-signal (queue-semaphore queue))))

(defun emptyp (queue)
  (lock (queue-mutex queue))
  (prog1 (null (queue-message-list queue))
    (unlock (queue-mutex queue))))

(defun queue-listify (queue)
  (lock (queue-mutex queue))
  (prog1 (copy-list (queue-message-list queue))
    (unlock (queue-mutex queue))))
