;;;; Check code projections; no interpreter behavior is replaced.
(require :asdf)
(load (merge-pathnames "../source-views.lisp" *load-truename*))
(let* ((root (truename (merge-pathnames "../../" (uiop:pathname-directory-pathname *load-truename*))))
       (scratch (merge-pathnames "build/source-view-tests/probe.abclr2" root))
       (count 0))
 (ensure-directories-exist scratch)
 (labels ((check (name actual expected)
            (assert (equal actual expected) () "SOURCE-VIEW-FAIL ~A" name)
            (incf count) (format t "SOURCE-VIEW-PASS ~A~%" name))
          (probe (name input active original)
            (with-open-file (out scratch :direction :output :if-exists :supersede
                                         :external-format :utf-8)
              (write-string input out))
            (check (list name :active) (abclr2-source-views:source-text scratch) active)
            (check (list name :original)
                   (abclr2-source-views:source-text scratch :original t) original)))
  (probe :no-final-newline "plain" "plain" "plain")
  (probe :replacement
         (format nil "before~%;; PORT-BEGIN: reason~%;;|old~%;; PORT-ACTIVE~%new~%;; PORT-END~%after")
         (format nil "before~%new~%after") (format nil "before~%old~%after"))
  (probe :addition
         (format nil ";; PORT-BEGIN: reason~%;; PORT-ACTIVE~%added~%;; PORT-END~%")
         (format nil "added~%") "")
  (probe :crlf
         (format nil ";; PORT-BEGIN: reason~C~C;;|old~C~C;; PORT-ACTIVE~C~Cnew~C~C;; PORT-END~C~C"
                 #\Return #\Newline #\Return #\Newline #\Return #\Newline
                 #\Return #\Newline #\Return #\Newline)
         (format nil "new~C~C" #\Return #\Newline)
         (format nil "old~C~C" #\Return #\Newline))
  (dolist (relative '("examples/misc/inherit.abclr2" "examples/misc/MCP.abclr2"
                      "examples/misc/meta-meta.abclr2" "examples/misc/fault-tolerant.abclr2"
                      "examples/time-warp/tw-eval.abclr2" "examples/time-warp/tw-meta.abclr2"
                      "lib/copy.abclr2" "lib/express.abclr2" "lib/report.abclr2"
                      "lib/heavy-meta.abclr2" "lib/heavy-meta-old.abclr2" "lib/express-old.abclr2"
                      "examples/misc/reify-args.abclr2" "examples/pcontrol/priori-meta.abclr2"
                      "examples/pcontrol/pqueue-eval.abclr2" "examples/time-warp/tw-meta-eval.abclr2"))
    (let* ((path (merge-pathnames relative root))
           (active (abclr2-source-views:source-text path))
           (original (abclr2-source-views:source-text path :original t)))
      (check relative (and (not (equal active original))
                           (not (search ";; PORT-" active))
                           (not (search ";; PORT-" original))) t)))
  (format t "source-views: ~D checks.~%" count)))
