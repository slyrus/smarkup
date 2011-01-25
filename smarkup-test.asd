
(asdf:defsystem :smarkup-test
  :name "smarkup-test"
  :author "Cyrus Harmon <cyrus@bobobeach.com>"
  :licence "BSD"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :description "S-Expression-based Markup Utilities"
  :depends-on (:ch-asdf :bibtex :smarkup)
  :components
  ((:module
    :test
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "smarkup-test" :depends-on (:defpackage))
     (:static-file "sample-bib" :pathname #p"sample.bib")
     (:static-file "sample-sexp" :pathname #p"sample.sexp")))))
