
#+sbcl (require :sb-introspect)

(asdf:defsystem :smarkup
  :name "smarkup"
  :author "Cyrus Harmon <cyrus@bobobeach.com>"
  :licence "BSD"
  :description "S-Expression-based Markup Utilities"
  :depends-on (:alexandria
               :cl-fad
               :named-readtables
               :asdf-objects
               :bibtex
               :cl-typesetting)
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "smarkup" :depends-on (:defpackage))
   (:cl-source-file "quote-reader-macro" :depends-on (:defpackage))
   (:cl-source-file "smarkup-asdf" :depends-on (:defpackage :smarkup :quote-reader-macro))
   (:cl-source-file "parameters" :depends-on (:defpackage))
   (:cl-source-file "bibliography" :depends-on (:defpackage :parameters))
   (:cl-source-file "filter"
                    :depends-on (:defpackage :parameters :bibliography))
   (:cl-source-file "xhtml-render"
                    :depends-on
                    (:defpackage :smarkup :filter :quote-reader-macro))
   (:cl-source-file "typesetting"
                    :depends-on
                    (:defpackage :smarkup :parameters :filter :quote-reader-macro))))
