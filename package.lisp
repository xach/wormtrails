;;;; package.lisp

(defpackage #:wormtrails
  (:use #:cl #:vecto #:geometry)
  (:shadowing-import-from #:vecto #:scale)
  (:export #:chart #:bucket #:thing
           #:*bucket-width*
           #:*bucket-gap*
           #:*sample-gap*
           #:*default-name*
           #:*font-file*
           #:*font-size*
           #:*canvas-padding*
           #:*text-padding*
           #:find-thing
           #:ensure-thing
           #:create-thing
           #:find-bucket
           #:ensure-bucket
           #:create-bucket
           #:find-sample
           #:create-sample
           #:ensure-sample
           #:add-data
           #:index
           #:bucket
           #:name
           #:screenbox
           #:value
           #:chart-label
           #:draw-label
           #:establish-colors
           #:height
           #:width
           #:output-html
           #:all-buckets
           #:only-top-samples
           #:samples
           #:best-label-sample
           #:rainbow-colored-mixin
           #:for-each-line
           #:random-range
           #:filename-only
           #:linear-scaler
           #:string-hash
           #:hash-range
           #:rgb-color
           #:rgba-color
           #:hsv-color
           #:html-code))

(in-package #:wormtrails)
