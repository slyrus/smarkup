
(in-package #:cl-user)

(defpackage #:smarkup
  (:use #:cl #:asdf #:ch-asdf)
  (:export #:object-from-file
           #:filtered-object
           #:object-latex-file
           #:pdf-file
           #:object-xhtml-file
           #:object-cl-pdf-file
           
           #:smarkup-object-from-file))

