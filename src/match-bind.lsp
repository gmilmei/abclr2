;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; ABCL/R2 system
;;; script pattern matching function
;;; Nov. 9, 1991 by MASUHARA, H.

(in-package "ABCLR2-SYSTEM")

;; Like match.lsp, matching pattern of script and message is
;; taken. But the returned value is codes that porduces
;; bindings.

;; (make-script-binding-function pattern)
;; A function which takes a message is created,
;; and it returns binding list if the messages
;; matches to the pattern, and nil is returned
;; when the messages does not match.

(defun expand-matching-bind (pattern value body result-holder)
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
      `(progn (push (cons (quote ,pattern) ,value)
                    ,result-holder)
              ,body)))
   ((consp pattern)
    (if (macro-function (car pattern))
      (expand-matching-bind (macroexpand pattern) value body result-holder)
      (let ((tmp (gensym)))
        `(let ((,tmp ,value))
           ,(ecase (car pattern)
                   (bracket (expand-matching-bind-list
			     (cdr pattern) tmp body result-holder))
                   (bracket* (expand-matching-bind-list*
			      (cdr pattern) tmp body result-holder))
                   (list (expand-matching-bind-list
			  (cdr pattern) tmp body result-holder))
                   (list* (expand-matching-bind-list*
			   (cdr pattern) tmp body result-holder))
                   (cons (expand-matching-bind-cons
			  (cdr pattern) tmp body result-holder))
                   (append (expand-matching-bind-append
			    (cdr pattern) tmp body result-holder)))))))
   (t (error "match: Illegal pattern ~S~%" pattern))))

(defun expand-matching-bind-list (pattern-list tmp body result-holder)
  (cond ((null pattern-list)
         `(if (null ,tmp) ,body))
        ((null (cdr pattern-list))
         `(and (consp ,tmp) (null (cdr ,tmp))
               ,(expand-matching-bind (car pattern-list) `(car ,tmp) body result-holder)))
        (t `(if (consp ,tmp)
              ,(expand-matching-bind
                (car pattern-list)
                `(pop ,tmp)
                (expand-matching-bind-list (cdr pattern-list)
                                           tmp body result-holder)
                result-holder)))))

(defun expand-matching-bind-list* (pattern-list tmp body result-holder)
  (cond ((null pattern-list)
         (error "match: list* requires more than zero arguments."))
        ((null (cdr pattern-list))
         (expand-matching-bind (car pattern-list) tmp body result-holder))
        (t `(if (consp ,tmp)
              ,(expand-matching-bind
                (car pattern-list)
                `(pop ,tmp)
                (expand-matching-bind-list* (cdr pattern-list)
                                            tmp body result-holder)
                result-holder)))))
(defun expand-matching-bind-cons (pattern-list tmp body result-holder)
  (if (/= (length pattern-list) 2)
    (error "match: cons requires two arguments."))
  `(if (consp ,tmp)
     ,(expand-matching-bind (first pattern-list) `(car ,tmp)
                            (expand-matching-bind (second pattern-list)
                                                  `(cdr ,tmp)
                                                  body result-holder)
                            result-holder)))

(defun expand-matching-bind-append (pattern-list tmp body)
  (error "match: append."))

(defmacro make-script-binding-function (pattern)
  (let ((result (gensym)) (value (gensym)))
    `#'(lambda (,value)
         (let ((,result nil))
           (if ,(expand-matching-bind pattern value t result)
             (or ,result t) nil)))))