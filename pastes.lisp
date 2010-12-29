;;;; pastes.lisp

(in-package #:wormtrails)

(defclass pastechart (chart rainbow-colored-mixin) ())

(defclass pastemonth (bucket) ())

(defclass pastenick (thing) ())

(defmethod create-thing (name (pastechart pastechart))
  (make-instance 'pastenick :name name))

(defmethod create-bucket (index (pastechart pastechart))
  (make-instance 'pastemonth :index index))

(defun load-chart (file)
  (let ((chart (make-instance 'pastechart)))
    (for-each-line (line file)
      (let* ((comma (position #\, line))
             (nick (subseq line 0 comma))
             (rest (subseq line (1+ comma)))
             (yearstring (subseq rest 0 4))
             (monthstring (subseq rest 5 7))
             (date (+ (* (parse-integer yearstring) 100)
                      (parse-integer monthstring))))
        (add-data chart date nick 1)))
    chart))
