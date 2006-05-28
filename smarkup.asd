

(asdf:operate 'asdf:load-op "ch-asdf")

(defpackage #:smarkup-system (:use #:cl #:asdf #:ch-asdf))
(in-package #:smarkup-system)

(defclass smarkup-cl-source-file (ch-asdf:ch-cl-source-file) ())

(defsystem :smarkup
  :name "smarkup"
  :author "Cyrus Harmon <cyrus@bobobeach.com>"
  :licence "BSD"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :description "S-Expression-based Markup Utilities"
  :depends-on (ch-asdf ch-util)
  :components
  ((:module
    :src
    :components
    ((:smarkup-cl-source-file "defpackage")
     (:smarkup-cl-source-file "smarkup" :depends-on (:defpackage))
     (:smarkup-cl-source-file "quote-reader-macro" :depends-on (:defpackage))))
   (:static-file "version" :pathname #p"version.lisp-expr")
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")))
