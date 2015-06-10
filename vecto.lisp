;;;; vecto.lisp

(in-package #:wormtrails)

;;; Specials

(defvar *font-file*
  (asdf:system-relative-pathname 'wormtrails "font.ttf"))
(defvar *font-size* 12)
(defvar *canvas-padding* 16)
(defvar *text-padding* 2)

;;; Drawing

(defmacro with-box-canvas (box &body body)
  (let ((box* (gensym "BOX")))
    `(let* ((,box* ,box))
       (with-canvas (:width (ceiling (width ,box*))
                            :height (ceiling (height ,box*)))
         (translate (- (xmin ,box*)) (- (ymin ,box*)))
         ,@body))))

(defun top-left (box)
  (point (xmin box) (ymax box)))

(defun top-right (box)
  (maxpoint box))

(defun bottom-left (box)
  (minpoint box))

(defun bottom-right (box)
  (point (xmax box) (ymin box)))

(defun line-to* (p)
  (line-to (x p) (y p)))

(defun move-to* (p)
  (move-to (x p) (y p)))

(defun box-rectangle (box)
  (rectangle (xmin box) (ymin box) (width box) (height box)))

(defun curve-to* (c1 c2 p)
  (curve-to (x c1) (y c1)
            (x c2) (y c2)
            (x p) (y p)))

(defun draw-string* (point string)
  (draw-string (x point) (y point) string))

(defun draw-centered-string* (point string)
  (draw-centered-string (x point) (y point) string))

(defun gentle-curve-to (p1 p2)
  (let ((offset (point (* 0.5 (- (x p2) (x p1))) 0)))
    (curve-to* (add p1 offset) (sub p2 offset) p2)))

(defun draw-box-glue (b1 b2)
  (move-to* (top-right b1))
  (gentle-curve-to (top-right b1) (top-left b2))
  (line-to* (bottom-left b2))
  (gentle-curve-to (bottom-left b2) (bottom-right b1))
  (close-subpath))

(defun connect-buckets (chart)
  (let ((count 0))
    (loop for (left . rest) on (all-buckets chart)
          for right = (first rest)
          while right do
          (dolist (left-sample (only-top-samples left))
            (let ((right-sample (find-top-sample (thing left-sample) right)))
              (when right-sample
                (incf count)
                (with-graphics-state
                  (set-fill-color* (color (thing left-sample)))
                  (draw-box-glue (screenbox left-sample)
                                 (screenbox right-sample))
                  (fill-path))))))
    count))

(defgeneric chart-label (object))

(defmethod chart-label ((thing thing))
  (name thing))

(defmethod chart-label ((bucket bucket))
  (format nil "~D" (index bucket)))


(defgeneric draw-label (object))

(defgeneric draw-background (object)
  (:method ((object t))))

(defun ellipsize (string font-size width)
  (let ((loader (get-font *font-file*)))
    (labels ((swidth (string)
               (width (bbox-box
                       (string-bounding-box string font-size loader))))
             (try (string)
               (when (< (swidth string) width)
                 (return-from ellipsize string))))
      (try string)
      (loop for i from (length string) downto 5
            do (try (format nil "~A..." (subseq string 0 i))))
      "")))

(defun first-label-fit (samples)
  (find-if (lambda (sample)
             (< 15 (height (screenbox sample))))
           samples))

(defgeneric best-label-sample (thing)
  (:method (thing)
    (first-label-fit (samples thing))))

(defmethod draw-label ((thing thing))
  (let ((sample (best-label-sample thing))
        (string (chart-label thing)))
    (when (and sample string)
      (let ((box (screenbox sample)))
        (when (< (+ *font-size* *text-padding*)
                 (height box))
          (setf string (ellipsize string
                                  *font-size*
                                  (width (contract box *text-padding*))))
          (with-graphics-state
            (set-fill-color* (add-alpha (contrasting-text-color (color thing))
                                        0.5))
            (box-text box
                      string
                      :padding (- *text-padding*)
                      :vertical :top :horizontal :left)))))))

(defmethod draw-label ((bucket bucket))
  (let ((string (chart-label bucket)))
    (when string
      (set-rgba-fill 0 0 0 0.5)
      (box-text (bounding-box bucket)
                string
                :padding *text-padding*
                :vertical -20
                :horizontal :center))))


(defun %box-text-point (box string &key loader size vertical horizontal)
  (let ((string-box (bbox-box (string-bounding-box string size loader)))
        (x 0)
        (y 0)
        (mid (centerpoint box)))
    (setf y (case vertical
              (:atop (ymax box))
              (:top (- (ymax box) size))
              (:center (- (y mid) (/ size 2)))
              (:bottom (ymin box))
              (:below (- (ymin box) size))
              (t
               (if (numberp vertical)
                   vertical
                   (error "Unknown vertical specification ~A" vertical)))))
    (setf x (ecase horizontal
              (:left (xmin box))
              (:right (- (xmax box) (xmax string-box)))
              (:center (- (x mid) (/ (xmax string-box) 2)))))
    (point x y)))
               

(defclass zchart (chart) ())

(defmethod establish-colors ((chart zchart))
  (dolist (thing (all-things chart))
    (setf (color thing) (rgb-color (random 1.0) (random 1.0) 0.1))))

(defun box-text (box string &key
                 (vertical :bottom) (horizontal :left)
                 (padding 0))
  (unless (zerop padding)
    (setf box (expand box padding)))
  (let* ((font (vecto::font vecto::*graphics-state*))
         (point (%box-text-point box string
                                 :loader (vecto::loader font)
                                 :size (vecto::size font)
                                 :vertical vertical
                                 :horizontal horizontal)))
    (draw-string* point string)))

(defun device-pixel (point)
  (multiple-value-bind (x y)
      (funcall (vecto::transform-function vecto::*graphics-state*)
               (x point)
               (y point))
    (point (round x) (round y))))

(defun urlencode (string)
  (with-output-to-string (stream)
    (map nil (lambda (c)
               (if (alphanumericp c)
                   (write-char c stream)
                   (format stream "%~2,'0X" (char-code c))))
         string)))

(defgeneric imagemap-link (object)
  (:method (object)))

(defgeneric imagemap-mouseover (object)
  (:method (object)))

(defun write-imagemap-area (sample stream)
  (let* ((box (screenbox sample))
         (min (device-pixel (minpoint box)))
         (max (device-pixel (maxpoint box))))
    (format stream "<area~@[ href='~A'~] target=_new shape=rect ~
                          ~@[ onmouseover='~A'~] ~
                          coords='~D,~D,~D,~D'>"
            (imagemap-link sample)
            (imagemap-mouseover sample)
            (x min)
            (y max)
            (x max)
            (y min))))

(defun write-imagemap (chart stream)
  (format stream "<map name=clmap>~%")
  (dolist (bucket (all-buckets chart))
    (dolist (sample (only-top-samples bucket))
      (write-imagemap-area sample stream)
      (terpri stream)))
  (format stream "</map>"))

(defgeneric write-html-header (chart stream)
  (:method (chart stream)))

(defgeneric write-html-footer (chart stream)
  (:method (chart stream)))

(defun save-html (chart image-file)
  ;; FIXME needs to include template header/trailer
  (let ((html-file (make-pathname :type "html"
                                  :defaults image-file)))
    (with-open-file (stream html-file
                     :direction :output :if-exists :supersede)
      (write-html-header chart stream)
      (format stream "<img src='~A' usemap='#clmap' border=0>"
              (filename-only image-file))
      (write-imagemap chart stream)
      (write-html-footer chart stream))))

(defun samples/100px (scale)
  (/ 100 scale))

(defun draw-metrics (chart height label &key (alignment :center))
  "Draw the shaded metric bars on the chart."
  (let* ((box (bounding-box chart))
         (expanded (expand box *canvas-padding*))
         (point (add (midpoint (top-left box) (bottom-left box))
                     (point (- *text-padding*) 0)))
         (center (centerpoint box)))
    (flet ((centered-bar (y)
             (rectangle (xmin expanded) (- y (/ height 2))
                        (width expanded) height)))
      ;; Draw the bars
      (with-graphics-state
        (set-rgba-fill 0 0 0 0.1)
        (dotimes (i 10)
          (centered-bar (+ (* height 2 i) (y center)))
          (centered-bar (+ (* height -2 i) (y center))))
        (fill-path))
      ;; Draw the label
      (with-graphics-state
        (set-rgba-fill 0 0 0 0.5)
        (translate (x point) (y point))
        (rotate (/ pi 2))
        (draw-centered-string* (point 0 0) label)))))


(defun output-html (chart png-file
                    &key (scaler 'identity)
                    (metric-height 100)
                    (metric-label "")
                    (alignment :center)
                    (labels t)
                    (top-n 10))
  (decimate chart top-n)
  (layout chart :screenbox-scaler scaler)
  (ecase alignment
    (:center
     (center-align chart))
    (:top
     (top-align chart))
    (:bottom))
  (establish-colors chart)
  (with-box-canvas (expand (bounding-box chart) *canvas-padding*)
    (draw-background chart)
    (save-html chart png-file)
    (set-font (get-font *font-file*) *font-size*)
    (when (and metric-height metric-label)
      (draw-metrics chart metric-height metric-label :alignment alignment))
    (dolist (bucket (all-buckets chart))
      (dolist (sample (only-top-samples bucket))
        (box-rectangle (screenbox sample))
        (set-fill-color* (color (thing sample)))
        (fill-path)))
    (connect-buckets chart)
    (when labels
      (dolist (thing (only-top-things chart))
        (when (samples thing) (draw-label thing)))
      (dolist (bucket (all-buckets chart))
        (draw-label bucket)))
    (save-png png-file)))
