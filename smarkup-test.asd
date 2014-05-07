
(asdf:defsystem :smarkup-test
  :name "smarkup-test"
  :author "Cyrus Harmon <cyrus@bobobeach.com>"
  :licence "BSD"
  :description "S-Expression-based Markup Utilities"
  :depends-on (:smarkup :bibtex)
  :components
  ((:module
    :test
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "smarkup-test" :depends-on (:defpackage))
     (:static-file "sample-bib" :pathname #p"sample.bib")
     (:static-file "sample-sexp" :pathname #p"sample.sexp")))))
