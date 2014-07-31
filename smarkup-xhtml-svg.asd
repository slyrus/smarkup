
(asdf:defsystem :smarkup-xhtml-svg
  :name "smarkup-xhtml-svg"
  :author "Cyrus Harmon <cyrus@bobobeach.com>"
  :licence "BSD"
  :description "XHTML SVG Extemsions for smarkup"
  :depends-on (:smarkup :cxml :cxml-stp)
  :serial t
  :components ((:cl-source-file "xhtml-svg-render")))
