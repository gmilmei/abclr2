;;;-*- Mode: Lisp; Package: ABCLR2-SYSTEM -*-
;;; **** Tentative version ****
;;;    Function names are replaced---in order to avoid incompatibility
;;;    with ACL 4.2 on IRIX (still not sure...).
;;;
;;; June 7, 1992:
;;;    A readtable --- *interactive-readtable* --- is introduced for
;;;    MCL 2.0b1, which cannot "peek-char" from interactive streams.
;;;    Current solution to this problem is that it does not use
;;;    "peek-char" when the toplevel object reads from console.
;;;    Therefore, you cannot input dotted-bracket expressions (e.g.,
;;;    [a b c . d]) interactively.

;;; Bracket [] and Brace {} reader macros
;;; Takuo Watanabe

(in-package "ABCLR2-SYSTEM")

(defvar *abcl-readtable*)


#+:CCL-2
(defvar *interactive-readtable*)

#|
(defun bracket? (exp)
  (and (consp exp) (eql (car exp) ':bkt)))
(defmacro bkt-first (bkt) `(cadr ,bkt))
(defmacro bkt-second (bkt) `(third ,bkt))
(defmacro bkt-cdr (bkt) `(cons ':bkt (cddr ,bkt)))
(defmacro bkt->list (bkt) `(cdr ,bkt))
(defmacro list->bkt (list) `(cons ':bkt ,list))

(defun brace? (exp)
  (and (consp exp) (eql (car exp) ':brc)))
(defmacro brc-first (brc) `(cadr ,brc))
(defmacro brc-second (brc) `(third ,brc))
(defmacro brc-cdr (brc) `(cons ':brc (cddr ,brc)))
(defmacro brc->list (brc) `(cdr ,brc))
(defmacro list->brc (list) `(cons ':brc ,list))
|#

(defun bracket-reader (stream macro-char)
  (declare (ignore macro-char))
  (let ((body (read-delimited-tree #\] stream t nil t)))
    (if (null (cdr (last body)))
      (cons 'bracket body)
      (progn 
        (setf (cdr (last body)) (list (cdr (last body))))
        (cons 'bracket* body)))))

(defun interactive-bracket-reader (stream macro-char)
  (declare (ignore macro-char))
  (cons 'bracket (read-delimited-list #\] stream t)))

;;  (cons ':bkt (read-delimited-tree #\] stream t nil t)))

(defun brace-reader (stream macro-char)
  (declare (ignore macro-char))
  (cons ':brc (read-delimited-tree #\} stream t nil t)))

(defun read-delimited-tree (limitchar &optional
                                      (input-stream *standard-input*)
                                      (eof-error-p t)
                                      (eof-value nil)
                                      (recursive-p nil)
                                      &aux
                                      (reslist nil))
  (do ((c (npeek-char input-stream eof-error-p eof-value recursive-p)
          (npeek-char input-stream eof-error-p eof-value recursive-p)))
      ((char= c limitchar)
       (read-char input-stream eof-error-p eof-value recursive-p)
       (nreverse reslist))
    (if (char= c #\.)
      (progn
        (if (null reslist)
          (error "Syntax"))
        (read-char input-stream eof-error-p eof-value recursive-p)
        (if (char= (npeek-char input-stream eof-error-p eof-value recursive-p)
                   limitchar)
          (error "Unexpected EOF")
          (let ((result (nconc (nreverse reslist)
                               (read input-stream eof-error-p eof-value recursive-p))))
            (if (char= (npeek-char input-stream eof-error-p eof-value recursive-p)
                       limitchar)
              (progn
                (read-char input-stream eof-error-p eof-value recursive-p)
                (return-from read-delimited-tree result))
              (error "Syntax")))))
      (push (read input-stream eof-error-p eof-value recursive-p)
            reslist))))

(defun npeek-char (&optional (input-stream *standard-input*)
                             (eof-error-p t)
                             (eof-value nil)
                             (recursive-p nil))
  #-(or :Coral Symbolics :CMU)
  (peek-char t input-stream eof-error-p eof-value recursive-p)
  #+(or Symbolics :CMU)
  (do ((c (peek-char t input-stream eof-error-p eof-value recursive-p)
	  (peek-char t input-stream eof-error-p eof-value recursive-p)))
      ((and (characterp c) (not (char= c #\;))) c)
    ;; skip comments
    (if (and (characterp c) (char= c #\;))
	(read-line input-stream eof-error-p eof-value recursive-p)))
  #+:Coral
  ;(if (eql input-stream ccl:*top-listener*)
    (do ((c (peek-char t input-stream nil nil recursive-p)
            (peek-char t input-stream nil nil recursive-p)))
        ((and (characterp c) (not (char= c #\;))) c)
      ;; skip comments
      (if (and (characterp c) (char= c #\;))
        (read-line input-stream nil nil recursive-p)))
    ;(peek-char t input-stream eof-error-p eof-value recursive-p))
    )

#+Symbolics
(defun unmatch-error (stream macro-char)
  (declare (ignore stream))
  (error "Unmatched parenthesis: ~C" macro-char))

;;; reader macros 

(defun single-cmac-reader (stream macro-char)
  (cond ((char= macro-char #\!)
         (list 'exclam (read stream t nil t)))
        (t (error "unknown macro character: ~C" macro-char))))

;;; backquote and comma for macro definitions
(defun backquote-reader (stream macro-char)
  (declare (ignore macro-char))
  (list :backquote (read stream t nil t)))

(defun comma-reader (stream macro-char)
  (declare (ignore macro-char))
  (let ((c (npeek-char stream t nil nil)))
    (if (char= c #\@)
      (progn (read-char stream t nil nil)
             (list :comma-at (read stream t nil t)))
      (list :comma (read stream t nil t)))))

#| 
o(eval-when (load eval)
  (copy-readtable *readtable* *abcl-readtable*) 
  (set-macro-character #\[ #'bracket-reader t *abcl-readtable*)
  #-Symbolics
  (set-macro-character #\] (get-macro-character #\)) *abcl-readtable*)
  #+Symbolics
  (set-macro-character #\] #'unmatch-error *abcl-readtable*)
  (set-macro-character #\{ #'brace-reader t *abcl-readtable*)
  #-Symbolics
  (set-macro-character #\} (get-macro-character #\)) *abcl-readtable*)
  #+Symbolics
  (set-macro-character #\} #'unmatch-error *abcl-readtable*)
  ;;
  (set-macro-character #\! #'single-cmac-reader t *abcl-readtable*)
  
  (setq *readtable* *abcl-readtable*)
  )
|#

;; Using *abcl-readtable* does not work properly in MACL.
;; Thus the default readtable is overwritten.

(defun init-abclr2-readtable ()
  (set-macro-character #\[ #'bracket-reader t)
  #-Symbolics
  (set-macro-character #\] (get-macro-character #\)))
  #+Symbolics
  (set-macro-character #\] #'unmatch-error)
  (set-macro-character #\{ #'brace-reader t)
  #-Symbolics
  (set-macro-character #\} (get-macro-character #\)))
  #+Symbolics
  (set-macro-character #\} #'unmatch-error)
  ;;
  (set-macro-character #\! #'single-cmac-reader t)
  ;;
  ;;(set-macro-character #\` #'backquote-reader t)
  ;;(set-macro-character #\, #'comma-reader t)
  #+:CCL-2
  (setq *interactive-readtable*
        (copy-readtable *readtable*))
  #+:CCL-2
  (set-macro-character #\[ #'interactive-bracket-reader t *interactive-readtable*)
  )

#| test
(setq test1 '[1 2 3
      ;; comment
      4 5 ; more comment
      ;; one more comment
      ])

(setq test2 '[1 2 3
      ;; comment
      . ; more comment
      ;; one more
      last
      ])
|#

