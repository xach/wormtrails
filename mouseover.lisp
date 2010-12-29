;;;; mouseover.lisp

(in-package #:wormtrails)

(defclass mouseover-mixin () ())

(defgeneric mouseover-banner-html (object)
  (:method (sample)
    (format nil "~A (~D)"
            (name (thing sample))
            (value sample))))

(defmethod write-html-header ((chart mouseover-mixin) stream)
  (html-template:fill-and-print-template #p"mouseover-header.html"  
                                         (list :title (name chart))
                                         :stream stream))
(defmethod imagemap-mouseover ((sample mouseover-mixin))
  (format nil "updateBanner(~S, ~S)"
          (mouseover-banner-html sample)
          (html-code (color (thing sample)))))