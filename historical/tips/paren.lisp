;;;-*- Mode: Lisp; Package: CCL -*-
;;; paren.lisp
;;; Electric parentheses for ABCL programming.  
;;; To use, just load this file.
;;; Mar. 22, 1993 by MASUHARA, H.

(in-package :CCL)

(export 'ELECTRIC-CLOSE-PAREN)

(defun my-buffer-bwd-up-sexp (buf &optional pos count start
                                  &aux ch eol-count temp-pos
                                  count-alist
                                  (match-alist '((#\) . #\()
                                                 (#\} . #\{)
                                                 (#\] . #\[))))
  "An extension of buffer-bwd-up-sexp, which is defined in
l1-edcmd.lisp.  In this function, multiple types of parentheses, such
as brackets or braces, are matched as well as ordinarly parentheses."
  ;Supposedly handles comments.
  (when (null pos)(setq pos (buffer-position buf)))
  (when (null count) (setq count 1))
  (setq count-alist (pairlis '(#\( #\{ #\[) (list count 1 1)))
  (when (null start) (setq start 0))
  (while (and (setq pos (buffer-backward-find-char buf "()[]{}|\";
" pos))
              (>= pos start))
    (unless (buffer-lquoted-p buf pos)
      (setq ch (buffer-char buf pos))
      (cond ((member ch '(#\) #\} #\]) :test #'eq)
             (incf (cdr (assoc (cdr (assoc ch match-alist))
                               count-alist))))
            ((member ch '(#\( #\{ #\[))
             (decf (cdr (assoc ch count-alist)))
             (when (some #'zerop (mapcar #'cdr count-alist))
               (if eol-count
                 (progn
                   (setq temp-pos (buffer-backward-find-char buf ";
" pos))
                   (if (and temp-pos 
                            (eq #\; (buffer-char buf temp-pos)))
                     (setq count eol-count pos temp-pos)
                     (return-from my-buffer-bwd-up-sexp pos)))
                 (return-from my-buffer-bwd-up-sexp pos))))
            ((eq ch #\newline) (setq eol-count count))
            ((eq ch #\;) (if eol-count
                           (setq count eol-count)
                           (return-from my-buffer-bwd-up-sexp nil)))
            (t (unless (setq pos (buffer-backward-search-unquoted buf ch pos))
                 (return-from my-buffer-bwd-up-sexp nil)))))))

(defun electric-close-paren (window)
  "A closing parenthesis corresponding to the current context is
selected then inserted.  If no appropriate parenthesis is found, it
just beeps.  If the user want to insert an unbalanced closing
parenthesis, C-q should be preceeded."
  (let* ((buf (fred-buffer window))
         (pos (buffer-position buf))
         (up-position (my-buffer-bwd-up-sexp buf pos)))
    (if up-position
      (let* ((char (buffer-char buf up-position))
             (closing-char 
              (cdr (assoc char '((#\( . #\)) (#\{ . #\}) (#\[ . #\]))
                          :test #'eq))))
        (and closing-char
             (ed-insert-with-undo window closing-char)))
      (progn (set-mini-buffer window "Mispatched parentheses.")
             (ed-beep)))))

(def-fred-command #\) electric-close-paren
  "Insert a matching closing parenthesis automatically.")
(def-fred-command #\] electric-close-paren
  "Insert a matching closing parenthesis automatically.")