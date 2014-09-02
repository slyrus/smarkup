
(in-package #:cl-user)

(defpackage #:smarkup
  (:use #:cl #:asdf #:asdf-objects)
  (:export #:*document-title*
           #:*document-titlerunning*
           #:*document-subtitle*
           #:*document-titlepage*
           #:*document-author*
           #:*document-tocauthor*
           #:*document-authorrunning*
           #:*document-address*
           #:*document-institute*
           #:*document-date*
           #:*document-css-stylesheet*

           #:render-as
           #:with-keyword-args

           #:object-from-file
           #:filtered-object
           #:object-latex-file
           #:pdf-file
           #:object-xhtml-file
           #:object-cl-pdf-file

           #:css-file
           #:xhtml-file
           #:tiff-file
           #:jpeg-file
           #:png-file

           #:smarkup-object-from-file))

