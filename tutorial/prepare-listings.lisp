;;;; Presentation-only source copies. Run with SBCL from the repository root.
;;;; Original files and executable test sources are never modified.
(load (merge-pathnames "source-views.lisp" *load-truename*))

(defun read-lines (path)
  (abclr2-source-views:source-lines path))

(defun read-text (path)
  (format nil "~{~A~%~}" (read-lines path)))

(defun split-at (text separator)
  (loop for start = 0 then (1+ end)
        for end = (position separator text :start start)
        collect (subseq text start end)
        while end))

(defun option-number (options name default)
  (let ((start (search (concatenate 'string name "=") options)))
    (if start
        (parse-integer options :start (+ start (1+ (length name)))
                               :junk-allowed t)
        default)))

(defun selected-lines (options count)
  (let ((selected (make-array count :initial-element nil))
        (ranges (search "linerange={" options)))
    (labels ((select-range (first last)
               (assert (<= 1 first last count) () "Invalid source range ~D-~D" first last)
               (loop for i from (1- first) below last do
                 (setf (aref selected i) t))))
      (if ranges
          (let* ((start (+ ranges (length "linerange={")))
                 (end (or (position #\} options :start start)
                          (error "Unclosed linerange: ~A" options))))
            (dolist (range (split-at (subseq options start end) #\,))
              (let ((pair (split-at range #\-)))
                (assert (= 2 (length pair)))
                (select-range (parse-integer (first pair))
                              (parse-integer (second pair))))))
          (select-range (option-number options "firstline" 1)
                        (option-number options "lastline" count))))
    selected))

(defun leading-width (line)
  (loop with width = 0
        for char across line
        for index from 0
        while (find char '(#\Space #\Tab))
        do (incf width (if (char= char #\Tab) (- 8 (mod width 8)) 1))
        finally (return (values width index))))

(defun content-line-p (line)
  (find-if-not (lambda (char) (find char '(#\Space #\Tab #\Return #\Page))) line))

(defun prepare-lines (lines options)
  (let* ((selected (selected-lines options (length lines)))
         (margin (loop for line in lines for i from 0
                       when (and (aref selected i) (content-line-p line))
                         minimize (leading-width line))))
    (loop for line in lines for i from 0
          collect (if (and (aref selected i) (content-line-p line))
                      (multiple-value-bind (width prefix) (leading-width line)
                        (concatenate 'string (make-string (- width margin)
                                                         :initial-element #\Space)
                                     (subseq line prefix)))
                      ""))))

(defun write-lines (path lines)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :external-format :utf-8)
    (dolist (line lines) (write-line line stream))))

;; The tutorial uses literal paths and literal listing options. This scanner
;; handles those project conventions, not arbitrary TeX macro expansion.
;; Recursive inputs preserve document order; comments and macro definitions
;; are skipped so the listing macros themselves are not treated as excerpts.
(defun scan-listings (path emit)
  (let* ((text (read-text path)) (size (length text)) (cursor 0))
    (labels ((group (open close)
               (assert (and (< cursor size) (char= (char text cursor) open)))
               (incf cursor)
               (let ((start cursor) (depth 1))
                 (loop while (< cursor size) do
                   (let ((char (char text cursor)))
                     (cond ((char= char open) (incf depth))
                           ((char= char close) (decf depth)))
                     (when (zerop depth)
                       (return-from group
                         (prog1 (subseq text start cursor) (incf cursor)))))
                   (incf cursor))
                 (error "Unclosed argument in ~A" path)))
             (skip-space ()
               (loop while (and (< cursor size)
                                (find (char text cursor) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf cursor))))
      (loop while (< cursor size) do
        (case (char text cursor)
          (#\% (setf cursor (or (position #\Newline text :start cursor) size)))
          (#\\
           (incf cursor)
           (let* ((start cursor)
                  (end (or (position-if-not #'alpha-char-p text :start cursor) size))
                  (command (subseq text start end)))
             (setf cursor end)
             ;; A control symbol such as \% is not a comment starter.
             (when (and (= start end) (< cursor size)) (incf cursor))
             (cond
               ((string= command "newcommand")
                (skip-space) (group #\{ #\})
                (skip-space)
                (when (and (< cursor size) (char= (char text cursor) #\[))
                  (group #\[ #\])
                  (skip-space)
                  (when (char= (char text cursor) #\[) (group #\[ #\])))
                (skip-space) (group #\{ #\}))
               ((member command '("wholelisting" "sourcelisting") :test #'string=)
                (skip-space)
                (let ((options (if (char= (char text cursor) #\[)
                                   (group #\[ #\]) "")))
                  (skip-space)
                  (funcall emit (group #\{ #\}) options)))
               ((string= command "input")
                (skip-space)
                (let ((input (group #\{ #\})))
                  (when (and (>= (length input) 9)
                             (string= input "tutorial/" :end1 9))
                    (scan-listings (if (pathname-type input) input
                                       (concatenate 'string input ".tex")) emit)))))))
          (otherwise (incf cursor)))))))

(defun check-listings (directory)
  ;; Tab stops, blank lines, and unselected control bytes must not leak into
  ;; TeX. These checks require no historical-source modification.
  (let ((tabbed (format nil "~C(foo)" #\Tab))
        (control (string (code-char 1))))
    (assert (equal (prepare-lines (list control tabbed "          (bar)" "  ")
                                 "firstline=2,lastline=4")
                   '("" "(foo)" "  (bar)" "")))
    (assert (equal (prepare-lines '("" "   ") "") '("" ""))))
  (dolist (case '(("examples/misc/inherit.abclr2" "linerange={33-35,38-48}" 33 8)
                  ("examples/misc/inherit.abclr2" "firstline=23,lastline=30" 23 2)
                  ("examples/pcontrol/qsort.abclr2" "firstline=54,lastline=62" 54 19)
                  ("tutorial/examples/03-jobs.abclr2" "firstline=2,lastline=14" 2 0)
                  ("examples/time-warp/tw-meta.abclr2" "linerange={96-101,103-105}" 96 30)))
    (destructuring-bind (source options first margin) case
      (let* ((original (read-lines source))
             (output (prepare-lines original options))
             (selected (selected-lines options (length original)))
             (path (merge-pathnames (format nil "check-~D.txt" first) directory)))
        (write-lines path output)
        (assert (equal output (read-lines path)))
        (assert (= (length original) (length output)))
        (loop for before in original for after in output for i from 0 do
          (if (aref selected i)
              (assert (string= (string-left-trim '(#\Space #\Tab) before)
                               (string-left-trim '(#\Space #\Tab) after)))
              (assert (string= after ""))))
        (assert (= margin (- (leading-width (nth (1- first) original))
                             (leading-width (nth (1- first) output)))))
        (format t "LISTING-PASS ~A ~A~%" source options))))
  ;; Check every displayed source range, not just the five tab/margin cases.
  (let ((seen (make-hash-table :test #'equal)) (count 0))
    (scan-listings
     "tutorial/tutorial.tex"
     (lambda (source options)
       (let ((key (list source options)))
         (unless (gethash key seen)
           (setf (gethash key seen) t)
           (let* ((original (read-lines source))
                  (output (prepare-lines original options))
                  (selected (selected-lines options (length original))))
             (assert (= (length original) (length output)))
             (assert (loop for line in original for i from 0
                           thereis (and (aref selected i) (content-line-p line))))
             (loop for before in original for after in output for i from 0 do
               (if (aref selected i)
                   (assert (string= (string-left-trim '(#\Space #\Tab) before)
                                    (string-left-trim '(#\Space #\Tab) after)))
                   (assert (string= after ""))))
             (incf count))))))
    (format t "LISTING-COVERAGE-PASS ~D source excerpts; code unchanged except indentation.~%"
            count)))

(let* ((args (cdr sb-ext:*posix-argv*))
       (directory (pathname (concatenate 'string (or (first args) "build/tutorial-build") "/"))))
  (ensure-directories-exist (merge-pathnames "listings.tex" directory))
  (if (member "--check" args :test #'string=)
      (check-listings directory)
      (let ((serial 0) (seen (make-hash-table :test #'equal)))
        (with-open-file (map (merge-pathnames "listings.tex" directory)
                             :direction :output :if-exists :supersede)
          (scan-listings
           "tutorial/tutorial.tex"
           (lambda (source options)
             (let ((key (format nil "~A|~A" source options)))
               (unless (gethash key seen)
                 (setf (gethash key seen) t)
                 (let ((path (merge-pathnames (format nil "listing-~D.txt" (incf serial))
                                              directory)))
                   (write-lines path (prepare-lines (read-lines source) options))
                   (format map "\\expandafter\\def\\csname listing:\\detokenize{~A}\\endcsname{~A}~%"
                           key (namestring path))))))))
        (format t "Prepared ~D source excerpts.~%" serial))))
