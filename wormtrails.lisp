;;;; wormtrails.lisp

(in-package #:wormtrails)

(defparameter *bucket-width* 50)
(defparameter *bucket-gap* 50)
(defparameter *sample-gap* 2)
(defparameter *default-name* "An unnamed chart")

(defclass chart ()
  ((name
    :initarg :name
    :accessor name)
   (buckets
    :initarg :buckets
    :accessor buckets)
   (things
    :initarg :things
    :accessor things)
   (bucket-width
    :initarg :bucket-width
    :accessor bucket-width
    :documentation "The width of an individual bucket's samples.")
   (bucket-gap
    :initarg :bucket-gap
    :accessor bucket-gap
    :documentation "The horizontal space between two consecutive buckets.")
   (sample-gap
    :initarg :sample-gap
    :accessor sample-gap
    :documentation "The vertical space between two consecutive sample boxes."))
  (:default-initargs
   :name *default-name*
   :buckets (make-hash-table)
   :things (make-hash-table :test 'equalp)
   :bucket-width *bucket-width*
   :bucket-gap *bucket-gap*
   :sample-gap *sample-gap*))

(defmethod print-object ((chart chart) stream)
  (print-unreadable-object (chart stream :type t :identity t)
    (write (name chart) :stream stream :readably t)))

(defgeneric all-things (container)
  (:method ((chart chart))
    (table-values (things chart))))

(defgeneric all-buckets (container)
  (:method ((chart chart))
    (sort (table-values (buckets chart)) #'< :key 'index)))

(defclass bucket ()
  ((index
    :initarg :index
    :accessor index)
   (samples
    :initarg :samples
    :accessor samples)
   (top-samples
    :initarg :top-samples
    :accessor top-samples))
  (:default-initargs
   :samples (make-hash-table)))

(defmethod print-object ((bucket bucket) stream)
  (print-unreadable-object (bucket stream :type t :identity t)
    (format stream "~D (~D sample~:P)" (index bucket)
            (hash-table-count (samples bucket)))))

(defgeneric all-samples (container)
  (:method ((bucket bucket))
    (sort (table-values (samples bucket)) #'< :key 'value)))

(defclass sample ()
  ((thing
    :initarg :thing
    :accessor thing)
   (screenbox
    :initarg :screenbox
    :accessor screenbox)
   (bucket
    :initarg :bucket
    :accessor bucket)
   (value
    :initarg :value
    :accessor value))
  (:default-initargs
   :value 0))


(defmethod print-object ((sample sample) stream)
  (print-unreadable-object (sample stream :type t :identity t)
    (format stream "~S => ~D"
            (name (thing sample))
            (value sample))))

(defclass thing ()
  ((name
    :initarg :name
    :accessor name)
   (color
    :initarg :color
    :accessor color)
   (samples
    :initarg :samples
    :accessor samples))
  (:default-initargs
   :samples nil
   :color (rgb-color 1.0 0.0 0.0)))

(defmethod print-object ((thing thing) stream)
  (print-unreadable-object (thing stream :type t :identity t)
    (write (name thing) :stream stream :readably t)))

(defgeneric total-value (object)
  (:method ((thing thing))
    (reduce #'+ (all-samples thing) :key #'value))
  (:method ((bucket bucket))
    (reduce #'+ (all-samples bucket) :key #'value)))


(defgeneric find-bucket (bucket-index chart)
  (:method (bucket-index chart)
    (gethash bucket-index (buckets chart))))

(defgeneric create-bucket (bucket-index chart)
  (:method (bucket-index chart)
    (make-instance 'bucket :index bucket-index)))

(defgeneric ensure-bucket (bucket-index chart)
  (:method (bucket-index chart)
    (or (find-bucket bucket-index chart)
        (setf (gethash bucket-index (buckets chart))
              (create-bucket bucket-index chart)))))

(defgeneric find-thing (name chart)
  (:method (name chart)
    (gethash name (things chart))))

(defgeneric create-thing (name chart)
  (:method (name chart)
    (make-instance 'thing :name name)))

(defgeneric ensure-thing (name chart)
  (:method (name chart)
    (or (find-thing name chart)
        (setf (gethash name (things chart))
              (create-thing name chart)))))

(defgeneric find-sample (thing bucket)
  (:method (thing bucket)
    (gethash thing (samples bucket))))

(defgeneric find-top-sample (thing bucket)
  (:method (thing bucket)
    (gethash thing (top-samples bucket))))

(defgeneric create-sample (thing bucket)
  (:method (thing bucket)
    (make-instance 'sample :thing thing :bucket bucket)))

(defgeneric ensure-sample (thing bucket)
  (:method (thing bucket)
    (or (find-sample thing bucket)
        (setf (gethash thing (samples bucket))
              (create-sample thing bucket)))))


(defgeneric add-data (chart index name value)
  (:method (chart index name value)
    (let* ((bucket (ensure-bucket index chart))
           (thing (ensure-thing name chart))
           (sample (ensure-sample thing bucket)))
      (incf (value sample) value)
      sample)))


(defmethod height ((sample sample))
  (value sample))

(defmethod height ((bucket bucket))
  (total-value bucket))

(defmethod height ((chart chart))
  (loop :for bucket :in (all-buckets chart)
     :maximize (height bucket)))

(defmethod width ((chart chart))
  (let ((bucket-count (hash-table-count (buckets chart))))
    (+ (* bucket-count (bucket-width chart))
       (* (1- bucket-count) (bucket-gap chart)))))

(defgeneric create-screenbox (sample chart scaler)
  (:method (sample chart scaler)
    (box 0 0 (bucket-width chart) (funcall scaler (height sample)))))

(defun displace (box point)
  (point-box (add point (minpoint box))
             (add point (maxpoint box))))

(defgeneric populate-samples (chart)
  (:method (chart)
    (dolist (thing (all-things chart))
      (setf (samples thing) nil))
    (dolist (bucket (reverse (all-buckets chart)))
      (dolist (sample (only-top-samples bucket))
        (push sample (samples (thing sample)))))))

(defgeneric layout (chart &key screenbox-scaler)
  (:method (chart &key (screenbox-scaler 'identity))
    (let ((base (point 0 0))
          (step (point (+ (bucket-width chart) (bucket-gap chart)) 0)))
      (populate-samples chart)
      (dolist (bucket (all-buckets chart))
        (let ((bucket-base base))
          (dolist (sample (only-top-samples bucket))
            (let ((screenbox (create-screenbox sample chart
                                               screenbox-scaler)))
              (setf (screenbox sample)
                    (displace screenbox bucket-base))
              (setf bucket-base (add bucket-base
                                     (point 0 (+ (height screenbox)
                                                 (sample-gap chart))))))))
        (setf base (add base step))))))

(defgeneric bounding-box (object)
  (:method ((list cons))
    (reduce #'combine list))
  (:method ((sample sample))
    (displace (screenbox sample) (neg (minpoint (screenbox sample)))))
  (:method ((bucket bucket))
    (bounding-box (mapcar #'screenbox (only-top-samples bucket))))
  (:method ((chart chart))
    (bounding-box
     ;; emarsden2011-01-16 for bottom gutter
     (append (list (box 0 -60 100 0))
             (mapcar #'bounding-box (all-buckets chart))))))

(defun align (chart style)
  (let ((scale (ecase style (:top 1.0) (:center 0.5)))
        (height (height (bounding-box chart))))
    (dolist (bucket (all-buckets chart))
      (let ((diff (- height (height (bounding-box bucket)))))
        (when (plusp diff)
          (let ((displacer (point 0 (* diff scale))))
            (dolist (sample (only-top-samples bucket))
              (setf (screenbox sample)
                    (displace (screenbox sample) displacer)))))))))

(defun top-align (chart)
  (align chart :top))

(defun center-align (chart)
  (align chart :center))

(defgeneric establish-colors (chart)
  (:method (chart)
    ;; Nothing by default
    ))

(defgeneric decimate (chart count)
  (:method (chart count)
    (dolist (bucket (all-buckets chart))
      (let ((new-table (make-hash-table)))
        (setf (top-samples bucket) new-table)
        (dolist (sample (first-n count (reverse (all-samples bucket))))
          (setf (gethash (thing sample) new-table) sample))))))

(defgeneric only-top-samples (container)
  (:method ((bucket bucket))
    (sort (table-values (top-samples bucket)) #'< :key #'value))
  (:method ((chart chart))
    (reduce #'nconc (mapcar #'only-top-samples (all-buckets chart)))))

(defgeneric only-top-things (container)
  (:method ((chart chart))
    (remove-duplicates (mapcar #'thing (only-top-samples chart)))))


;;; Rainbow coloring

(defclass rainbow-colored-mixin ()
  ((rainbow-steps
    :initarg :rainbow-steps
    :accessor rainbow-steps))
  (:default-initargs
   :rainbow-steps 1.0))

(defmethod establish-colors ((chart rainbow-colored-mixin))
  (let* ((buckets (all-buckets chart))
         (step (/ 360.0 (/ (length buckets) (rainbow-steps chart))))
         (count -1))
    (dolist (bucket buckets)
      (incf count)
      (dolist (sample (only-top-samples bucket))
        (let ((thing (thing sample)))
          (let* ((debut (first (samples thing)))
                 (start (* step count))
                 (end (+ start step))
                 (hash1 (hash1 (name thing)))
                 (hash2 (hash2 (name thing))))
            (when (eql debut sample)
              (setf (color thing) (hsv-color (hash-range start hash1 end)
                                             (hash-range 0.7 hash2 1.0)
                                             (hash-range 0.8 hash2 1.0))))))))))

(defmacro define-chart-methods (chart-class bucket-class
                                sample-class thing-class)
  `(progn
     (defmethod create-bucket (index (chart ,chart-class))
       (make-instance ',bucket-class :index index))
     (defmethod create-sample (thing (bucket ,bucket-class))
       (make-instance ',sample-class :thing thing :bucket bucket))
     (defmethod create-thing (name (chart ,chart-class))
       (make-instance ',thing-class :name name))))

