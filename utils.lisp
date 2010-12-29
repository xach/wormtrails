;;;; utils.lisp

(in-package #:wormtrails)

(defun call-for-each-line (file fun)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line do (funcall fun line))))

(defmacro for-each-line ((var file) &body body)
  `(call-for-each-line ,file
                       (lambda (,var) ,@body)))

(defun random-range (low high)
  (+ low (random (- high low))))

(defun first-n (n sequence)
  (if (< n (length sequence))
      (subseq sequence 0 n)
      sequence))

(defun table-values (table)
  (loop for v being each hash-value in table collect v))

(defun table-keys (table)
  (loop for k being each hash-key in table collect k))

(defun table-alist (table)
  (let ((result '()))
    (maphash (lambda (k v)
               (setf result (acons k v result)))
             table)
    result))

(defun filename-only (pathname)
  (let ((merged (merge-pathnames pathname)))
    (enough-namestring merged merged)))

(defun log-scaler (&optional (base 10))
  (lambda (v) (log v base)))

(defun linear-scaler (scale)
  (lambda (v) (* v scale)))

(defun hash1 (string)
  ;; From SBCL's %SXHASH-SUBSTRING
  (macrolet ((set-result (form)
               `(setf result (ldb (byte 32 0) ,form))))
    (let ((result 0)
          (octets (sb-ext:string-to-octets string :external-format :utf8)))
      (dotimes (i (length octets))
        (set-result (+ result (aref octets i)))
        (set-result (+ result (ash result 10)))
        (set-result (logxor result (ash result -6))))
      (set-result (+ result (ash result 3)))
      (set-result (logxor result (ash result -11)))
      (set-result (logxor result (ash result 15)))
      (/ result (expt 2.0 32)))))

(defun hash2 (string)
  (hash1 (concatenate 'string
                      (reverse string)
                      string)))

(defun hash-range (low hash high)
  (+ low (* hash (- high low))))


(defun htest (s1 s2)
  (format t "~A: ~15T  ~1,5F ~25T~1,5F~%" s1 (hash1 s1) (hash2 s1))
  (format t "~A: ~15T  ~1,5F ~25T~1,5F~%" s2 (hash1 s2) (hash2 s2)))

(defun truncate-time (time resolution)
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time time 0)
    (tagbody
       (ecase resolution
         (:quarter (go :quarter))
         (:year (go :year))
         (:month (go :month))
         (:day (go :day))
         (:hour (go :hour))
         (:minute (go :minute)))
     :quarter
       (setf month (1+ (* 3 (truncate (1- month) 3))))
       (go :month)
     :year
       (setf month 1)
     :month
       (setf date 1)
     :day
       (setf hour 0)
     :hour
       (setf minute 0)
     :minute
       (setf second 0)
     :end)
    (encode-universal-time second minute hour date month year 0)))

(defun parse-8601-time (string &key (start 0) end)
  (setf end (or end (length string)))
  (flet ((integer-at (pos length &optional (default 0))
           (let* ((s2 (+ start pos))
                  (e2 (+ s2 length)))
             (if (<= e2 end)
                 (parse-integer string :start s2 :end e2)
                 default))))
    (let ((year (integer-at 0 4))
          (month (integer-at 5 2 1))
          (day (integer-at 8 2 1))
          (hour (integer-at 11 2))
          (minute (integer-at 14 2))
          (second (integer-at 17 2)))
      (encode-universal-time second minute hour day month year))))
          
      

(defun pretty-quarter (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (let ((quarter (1+ (truncate (1- month) 3))))
      (format nil "~DQ~D" year quarter))))

(defparameter *months* #("JAN" "FEB" "MAR"
                         "APR" "MAY" "JUN"
                         "JUL" "AUG" "SEP"
                         "OCT" "NOV" "DEC"))

(defun pretty-month (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time 0)
    (declare (ignore second minute hour date))
    (format nil "~A ~D" (aref *months* (1- month)) year)))