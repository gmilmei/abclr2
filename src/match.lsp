;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; match - is macro
;;; Thanks to Yuji Ichisugi.
;;; Modified:Sep.27,1992:HM: _ is to be removed from let list.
;;; Modified:May.11,1994:HM: (let ((body))...) is fixed for CLISP

(in-package "ABCLR2-SYSTEM")

(defun get-pattern-var-list (pattern)
  (cond ((constantp pattern) nil)
	((and (symbolp pattern) (not (eq pattern '&)) (not (eq pattern '_)))
	 (list pattern))
	((and (consp pattern)
	      #+COMMENT
	      (memq (car pattern) '(bracket bracket* list list* cons append)))
	 (mapcan #'get-pattern-var-list (cdr pattern)))
	(t				; syntax error
	 nil)))

(defun expand-is-clause (is-clause block key)
  (if (or (< (length is-clause) 2)
	  (not (eq (car is-clause) 'is)))
      (error "Illegal clause for match: ~S" is-clause))
  (let ((pattern (second is-clause))
	(forms (cddr is-clause))
	(body nil))
    (setq body
	  (if (eq (car forms) 'where)
	      `(if ,(second forms) (return-from ,block (progn ,@(cddr forms))))
	      `(return-from ,block (progn ,@forms))))
    `(let ,(delete-duplicates (get-pattern-var-list pattern))
       ,(expand-matching-setq pattern key body))))
       

(defmacro match (key-form &rest clauses
			  &aux otherwise
			  (block (gensym))
			  (key (gensym)))
  "Syntex of match:
 (match keyform {(is pattern {form}*) | (otherwise {form}*)}*)"
  (let ((lastelm (car (last clauses))))
    (if (eq (car lastelm) 'otherwise)
	(progn (setq otherwise (cdr lastelm))
	       (setq clauses (butlast clauses)))
	(progn (setq otherwise 'nil))))
  `(block ,block
     (let ((,key ,key-form))
       ,@(mapcar #'(lambda (clause)
		     (funcall #'expand-is-clause clause block key))
		 clauses)
       ,@otherwise)))

(defmacro matching-setq (pattern value
				 )
  "Omake macro. returns nil iff pattern do not match to the value."
  (expand-matching-setq pattern value 't))

(defun expand-matching-setq (pattern value body)
  (cond
    ((constantp pattern)
     `(if (
	   ;; constant wa equal de check.
	   ;; symbol wa eq de, number, char ha eql de check.
	   ,(let ((const (eval pattern)))
	      (cond (#-ELIS
		     (symbolp const)
		     #+ELIS
		     (or (null const) (symbolp const))
		     'eq)
		    ((or (numberp const) (characterp const)) 'eql)
		    (t 'equal)))
	    ,pattern ,value)
	  ,body))
    ((symbolp pattern)
     (if (eq pattern '_)
	 `(progn ,value ,body)
	 `(progn (setq ,pattern ,value) ,body)))
    ((consp pattern)
     (if (macro-function (car pattern))
	 (expand-matching-setq (macroexpand pattern) value body)
	 (let ((tmp (gensym)))
	   `(let ((,tmp ,value))
	      ,(ecase (car pattern)
		 (bracket (expand-matching-setq-list (cdr pattern) tmp body))
		 (bracket* (expand-matching-setq-list* (cdr pattern) tmp body))
		 (list (expand-matching-setq-list (cdr pattern) tmp body))
		 (list* (expand-matching-setq-list* (cdr pattern) tmp body))
		 (cons (expand-matching-setq-cons (cdr pattern) tmp body))
		 (append (expand-matching-setq-append (cdr pattern) tmp body)))))))
    (t (error "match: Illegal pattern ~S~%" pattern))))

(defun expand-matching-setq-list (pattern-list tmp body)
  (cond ((null pattern-list)
	 `(if (null ,tmp) ,body))
	((null (cdr pattern-list))
	 `(and (consp ,tmp) (null (cdr ,tmp))
	       ,(expand-matching-setq (car pattern-list) `(car ,tmp) body)))
	(t `(if (consp ,tmp)
		,(expand-matching-setq
		  (car pattern-list)
		  `(pop ,tmp)
		  (expand-matching-setq-list (cdr pattern-list)
					     tmp body))))))

(defun expand-matching-setq-list* (pattern-list tmp body)
  (cond ((null pattern-list)
	 (error "match: list* requires more than zero arguments."))
	((null (cdr pattern-list))
	 (expand-matching-setq (car pattern-list) tmp body))
	(t `(if (consp ,tmp)
		,(expand-matching-setq
		  (car pattern-list)
		  `(pop ,tmp)
		  (expand-matching-setq-list* (cdr pattern-list)
					     tmp body))))))
(defun expand-matching-setq-cons (pattern-list tmp body)
  (if (/= (length pattern-list) 2)
      (error "match: cons requires two arguments."))
  `(if (consp ,tmp)
       ,(expand-matching-setq (first pattern-list) `(car ,tmp)
			      (expand-matching-setq (second pattern-list)
						    `(cdr ,tmp)
						    body))))

(defun expand-matching-setq-append (pattern-list tmp body)
  (error "match: append."))

;;; match-loop

(defmacro match-loop (&rest forms)
  "
Syntax: (match-loop key {(is pattern {body}*)}*
                        [(otherwise {body}*)])
"
  (if (eq (car (car (last forms))) 'otherwise)
      `(loop (match ,@forms))
      `(loop (match ,@forms (otherwise (return))))))


;;; -------------------------------------------------------------

