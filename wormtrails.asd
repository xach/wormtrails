;;;; wormtrails.asd

(asdf:defsystem #:wormtrails
  :depends-on (#:html-template
               #:cl-ppcre
               #:geometry
               #:vecto
               #:zpb-ttf)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "color")
               (:file "wormtrails")
               (:file "vecto")
               (:file "irc")
               (:file "referers")
               (:file "pastes")
               (:file "mouseover")
               (:file "webkit")
               (:file "sbcl")))

                 
