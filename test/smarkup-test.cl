
(in-package #:smarkup-test)

(defun test-xhtml-file ()
  (let ((smarkup-file (ch-asdf:asdf-lookup-path "asdf:/smarkup-test/test/sample-sexp"))
        (xhtml-file (ch-asdf:asdf-lookup-path "asdf:/smarkup-test/test/sample-xhtml")))
    (with-open-file (stream smarkup-file)
      (let ((sexp (read stream)))
        (let ((filtered (smarkup::apply-filters
                         sexp
                         '(:lisp :smarkup-metadata :html-metadata :ref))))
          (smarkup::render-as :xhtml filtered xhtml-file))))))
