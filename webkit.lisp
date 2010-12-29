;;;; webkit.lisp

(in-package #:wormtrails)

(defclass webkit-chart (chart mouseover-mixin rainbow-colored-mixin) ())

(defclass webkit-sample (sample mouseover-mixin) ())

(defclass webkit-bucket (bucket) ())

(defclass committer (thing) ())

(defmethod create-bucket (index (chart webkit-chart))
  (make-instance 'webkit-bucket :index index))

(defmethod create-sample (thing (bucket webkit-bucket))
  (make-instance 'webkit-sample :thing thing :bucket bucket))

(defparameter *webkit-revision-pattern*
  (cl-ppcre:create-scanner "^r\\d+ \\|"))

(defun parse-commit-line (line)
  (let* ((bar1 (position #\| line))
         (at1 (position #\@ line :start (1+ bar1)))
         (bar2 (position #\| line :start (1+ bar1)))
         (bar3 (position #\| line :start (1+ bar2))))
    (values (subseq line (+ bar1 2) (or at1 (- bar2 1)))
            (parse-8601-time (subseq line (+ bar2 2) (- bar3 1))))))

(defun load-chart (file)
  (let ((chart (make-instance 'webkit-chart)))
    (for-each-line (line file)
      (when (cl-ppcre:scan *webkit-revision-pattern* line)
        (multiple-value-bind (username time)
            (parse-commit-line line)
          (add-data chart (truncate-time time :quarter) username 1))))
    chart))

(defmethod mouseover-banner-html ((sample webkit-sample))
  (format nil "~A (~:D commit~:P)"
          (name (thing sample))
          (value sample)))

(defmethod chart-label ((bucket webkit-bucket))
  (pretty-quarter (index bucket)))

