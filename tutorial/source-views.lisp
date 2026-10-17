;;;; Read inline port notes without changing the source files.
(defpackage :abclr2-source-views (:use :cl) (:export :source-text :source-lines))
(in-package :abclr2-source-views)

(defun source-text (path &key original)
  "Select executable code, or reconstruct the commented original code.
PORT blocks contain ;;| original lines, then PORT-ACTIVE and the correction.
Outside these blocks all characters, including line endings, are retained."
  (with-open-file (in path :external-format :utf-8)
    (with-output-to-string (out)
      (let ((state :outside))
        (loop
          (multiple-value-bind (line missing-newline) (read-line in nil)
            (unless line (return))
            (let ((key (string-right-trim (string #\Return) line)))
              (flet ((emit (value)
                       (write-string value out)
                       (unless missing-newline (terpri out))))
                (cond
                  ((and (>= (length line) 14)
                        (string= line ";; PORT-BEGIN:" :end1 14))
                   (assert (eq state :outside)) (setf state :original))
                  ((string= key ";; PORT-ACTIVE")
                   (assert (eq state :original)) (setf state :active))
                  ((string= key ";; PORT-END")
                   (assert (eq state :active)) (setf state :outside))
                  ((eq state :original)
                   (assert (and (>= (length line) 3)
                                (string= line ";;|" :end1 3)))
                   (when original (emit (subseq line 3))))
                  ((or (eq state :outside) (not original)) (emit line)))))))
        (assert (eq state :outside))))))

(defun source-lines (path &key original)
  (with-input-from-string (in (source-text path :original original))
    (loop for line = (read-line in nil) while line collect line)))
