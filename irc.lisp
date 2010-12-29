;;;; irc.lisp

(in-package #:wormtrails)

(defclass irc (chart) ()
  (:default-initargs))

(defclass hour (bucket) ())

(defclass irc-sample (sample) ())

(defclass nick (thing) ())

(defmethod create-thing (name (chart irc))
  (make-instance 'nick :name name))

(defmethod create-bucket (index (chart irc))
  (make-instance 'hour :index index))

(defmethod create-sample (thing (hour hour))
  (make-instance 'irc-sample :thing thing :bucket hour))

(defparameter *irc-scanner*
  (cl-ppcre:create-scanner "^(\\d\\d):.. <(.*?)>")
  "Match IRC lines of the form \"HH:MM:SS <nickname> ...\"")

(defun parse-irc-line (line)
  (cl-ppcre:register-groups-bind (hour-string nick)
      (*irc-scanner* line)
    (when hour-string
      (values (parse-integer hour-string) nick))))
      
(defmethod chart-label ((hour hour))
  (let ((hour (index hour)))
    (cond ((= hour 0)
           "MIDNIGHT")
          ((= hour 12)
           "NOON")
          ((< hour 12)
           (format nil "~DAM" hour))
          (t
           (format nil "~DPM" (- hour 12))))))

(defun shade (start string end)
  (let ((h1 (hash1 string))
        (h2 (hash2 string)))
    (hsv-color (hash-range start h1 end)
               (hash-range 0.7 h2 1.0)
               (hash-range 0.3 h2 1.0))))

(defun red-shade (string)
  (shade 300 string 330))

(defun blue-shade (string)
  (shade 240 string 260))

(defun green-shade (string)
  (shade 15 string 50))

(defun maximum (list &key (key 'identity) (test '<))
  (when list
    (let* ((max (first list))
           (maxkey (funcall key max)))
      (dolist (elt (rest list) max)
        (let ((test-key (funcall key elt) ))
          (when (funcall test maxkey test-key)
            (setf max elt
                  maxkey test-key)))))))

(defun highest-activity (nick)
  (maximum (samples nick) :key 'value))

(defun total-activity (nick)
  (reduce #'+ (samples nick)))

(defun nick-color (nickname active-hour)
  (cond ((<= 7 active-hour 14)
         (blue-shade nickname))
        ((<= 15 active-hour 22)
         (green-shade nickname))
        (t
         (red-shade nickname))))

(defmethod best-label-sample ((nick nick))
  (highest-activity nick))

(defmethod establish-colors ((chart irc))
  (dolist (nick (only-top-things chart))
    (let ((sample (highest-activity nick)))
      (when sample
        (let ((bucket (bucket sample)))
          (setf (color nick)
                (nick-color (name nick) (index bucket))))))))

(defun irc-chart (input-file output-file)
  (let ((chart (make-instance 'irc))
        (scale 0.1))
    (for-each-line (line input-file)
      (multiple-value-bind (hour nick)
          (parse-irc-line line)
        (when hour
          (add-data chart hour nick 1))))
    (output-html chart output-file
                 :scaler (linear-scaler scale)
                 :metric-height 100 :metric-label "1K LINES")))

(defmethod imagemap-mouseover ((sample irc-sample))
  (format nil "updateBanner(~S, ~S)"
          (format nil "~A (~D)"
                  (name (thing sample))
                  (value sample))
          (html-code (color (thing sample)))))

(defmethod write-html-header ((chart irc) stream)
  (html-template:fill-and-print-template #p"irc-header.html"  
                                         (list :channel "#lisp")
                                         :stream stream))




