;;;; referers.lisp

(in-package #:wormtrails)

(defclass referers (chart rainbow-colored-mixin)
  ()
  (:default-initargs
   :rainbow-steps 1))

(defclass hourbucket (bucket) ())

(defclass linksample (sample) ())

(defclass referer (thing)
  ((linktable
    :initform (make-hash-table :test 'equalp)
    :reader linktable)))

(defmethod create-thing (name (chart referers))
  (make-instance 'referer :name name))

(defmethod create-sample (thing (bucket hourbucket))
  (make-instance 'linksample :thing thing :bucket bucket))

(defmethod create-bucket (index (chart referers))
  (make-instance 'hourbucket :index index))

(defun add-link (referer link)
  (incf (gethash link (linktable referer) 0)))

(defun best-link (referer)
  (first (maximum (table-alist (linktable referer)) :key 'cdr)))

(defun parse-url (url)
  (setf url (string-trim " " url))
  (when (string= url "")
    (error "Syntax error in url: ~A" url))
  (let ((i (mismatch url "http://" :test 'char-equal))
        (state :in-host)
        mark
        input
        host
        (port "80")
        path)
    (flet ((mark (&optional (delta 0)) (setf mark (+ i delta)))
           (save (&optional (pos i)) (subseq url mark pos)))
      (mark)
      (loop
       (when (<= (length url) i)
         (case state
           (:in-host
            (setf host (save) port "80" path "/"))
           (:in-port
            (setf port (save) path "/"))
           (:in-path
            (setf path (save))))
         (return (values host (parse-integer port) path)))
       (setf input (char url i))
       (case state
         (:in-host
          (cond ((char= input #\/)
                 (setf host (save))
                 (mark)
                 (setf state :in-path))
                ((char= input #\:)
                 (setf host (save))
                 (mark 1)
                 (setf state :in-port))
                ((not (position input
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."
                                :test 'char-equal))
                 (error "Invalid hostname in URL: ~A" url))))
         (:in-port
          (case input
            (#\/
               (setf port (save))
               (mark)
               (setf state :in-path))
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
            (t (error "Bad input in port: ~A" input))))
         (:in-path
          (case input
            (#\#
               (setf path (save)
                     state :in-fragment))))
         (:in-fragment))
       (incf i)))))

(defun canonicalize-referer (referer)
  "Convert a full URL to just the domain name for a referer. If the
referer can't be parsed, return NIL."
  (let ((host (or (ignore-errors (parse-url referer)) "-")))
    (when host
      (let* ((dot1 (position #\. host :from-end t))
             (dot2 (position #\. host :end dot1 :from-end t)))
        (if (and dot1 dot2)
            (subseq host (1+ dot2))
            host)))))

(defun nth-position (n char string)
  (let ((pos 0))
    (dotimes (i n pos)
      (when (= pos (length string))
        (return))
      (setf pos (position char string :start (1+ pos)))
      (unless pos
        (return)))))

(defun referer (logline)
  (let* ((start (nth-position 3 #\" logline))
         (end (position #\" logline :start (1+ start))))
    (when (and start end)
      (subseq logline (1+ start) end))))

(defun universal-time (logline)
  (let* ((pos (position #\[ logline)))
    ;; [03/Aug/2008:01:01:03 +0000]
    (flet ((number-at (start length)
             (let* ((s (+ 1 pos start))
                    (end (+ s length)))
               (parse-integer logline :start s :end end))))
      (let ((day (number-at 0 2))
            (year (number-at 7 4))
            (hour (number-at 12 2))
            (minute (number-at 15 2))
            (second (number-at 18 2))
            (month (if (char= (char logline (+ pos 4)) #\A)
                     8
                     7))
            (tz (- (number-at 21 3))))
        (encode-universal-time second minute hour day month year tz)))))

(defparameter *start-date*
  (encode-universal-time 0 0 0 4 8 2008))

(defun hourly-distance (time)
  (truncate (- time *start-date*) 3600))





(defvar *months* #(nil
                   "JAN"
                   "FEB"
                   "MAR"
                   "APR"
                   "MAY"
                   "JUN"
                   "JUL"
                   "AUG"
                   "SEP"
                   "OCT"
                   "NOV"
                   "DEC"))

(defun pretty-date (universal-time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format nil "~D ~A ~D" date (aref *months* month) year)))


(defmethod chart-label ((bucket hourbucket))
  (let* ((time (index bucket))
         (hour (nth-value 2 (decode-universal-time time))))
    (if (zerop hour)
        (format nil "~A" (pretty-date time))
        (format nil "~2,'0D:00" hour))))

(defun stringtime (string)
  (flet ((integer-at (start length)
           (if (<= (+ start length) (length string))
               (parse-integer string :start start :end (+ start length))
               0)))
    ;; 2008-01-01T00:00:00 -- using local zone
    (let ((year (integer-at 0 4))
          (month (integer-at 5 2))
          (day (integer-at 8 2))
          (hour (integer-at 11 2))
          (minute (integer-at 14 2))
          (second (integer-at 17 2)))
      (encode-universal-time second minute hour day month year))))

(defun timestring (time)
  (multiple-value-bind (second minute hour date month year day daylightp zone)
      (decode-universal-time time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))
          
            
(defun truncate-to-hour (time)
  (multiple-value-bind (second minute hour date month year day daylightp zone)
      (decode-universal-time time 0)
    (declare (ignore second minute day daylightp))
    (encode-universal-time 0 0 hour date month year 0)))


(defun referer-chart (logfile pngfile &key
                      (scale 0.05)
                      start
                      duration
                      thumb
                      (title "Referers"))
  (let* ((chart (make-instance 'referers :name title))
         (start (stringtime start))
         (end (+ start (* 3600 (1+ duration)))))
    (block nil
      (for-each-line (logline logfile)
        (let ((logtime (universal-time logline))
              (referer (referer logline)))
          (when (<= end logtime)
            (return))
          (when (and referer (string/= referer "-")
                     (< start logtime))
            (let* ((canon (canonicalize-referer referer))
                   (entry (ensure-thing canon chart)))
              (add-link entry referer)
              (add-data chart (truncate-to-hour logtime)
                        canon 1))))))
    (output-html chart pngfile :scaler (linear-scaler scale)
                 :metric-height 50
                 :metric-label "1K HITS")
    (when thumb
      (output-html chart "/tmp/thumb.png" :scaler (linear-scaler scale)
                   :metric-height nil
                   :labels nil))))


(defmethod write-html-header ((chart referers) stream)
  (html-template:fill-and-print-template #p"referer-header.html"  
                                         (list :title "Movie Chart Referral Trends")
                                         :stream stream))

(defmethod write-html-footer ((chart referers) stream)
  (html-template:fill-and-print-template #p"referer-footer.html"  nil
                                         :stream stream))

(defmethod imagemap-mouseover ((sample linksample))
  (format nil "updateBanner(~S, ~S)"
          (format nil "~A (~:D)"
                  (name (thing sample))
                  (value sample))
          (html-code (color (thing sample)))))

(defmethod imagemap-link ((sample linksample))
  (best-link (thing sample)))
