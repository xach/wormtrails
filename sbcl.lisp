;;;; sbcl.lisp

(in-package #:wormtrails)

(defclass sbcl-chart (chart rainbow-colored-mixin mouseover-mixin) ())

(defclass sbcl-bucket (bucket) ())

(defclass sbcl-sample (sample mouseover-mixin) ())

(defclass sbcl-committer (thing) ())

(define-chart-methods sbcl-chart sbcl-bucket sbcl-sample sbcl-committer)

(defvar *sbcl-line*
  "date: 2005/11/06 02:12:24;  author: jsnell;  state: Exp;  lines: +3 -4")

(defun parse-sbcl-line (line)
  (let* ((author-start (+ (length "author: ") (search "author: " line)))
         (author-end (position #\; line :start author-start))
         (time (parse-8601-time line :start 6)))
    (values (subseq line author-start author-end)
            time)))

(defun sbcl-chart (infile outfile &key
                   (scale 0.1)
                   (thumb nil)
                   (start-time 0)
                   (end-time (get-universal-time)))
  (let ((chart (make-instance 'sbcl-chart)))
    (for-each-line (line infile)
      (multiple-value-bind (author time)
          (parse-sbcl-line line)
        (when (<= start-time time end-time) 
          (add-data chart (truncate-time time :quarter) author 1))))
    (output-html chart outfile
                 :scaler (linear-scaler scale)
                 :metric-height (and (not thumb) 100)
                 :labels (not thumb)
                 :top-n 15)))

(defmethod chart-label ((bucket sbcl-bucket))
  (pretty-quarter (index bucket)))
