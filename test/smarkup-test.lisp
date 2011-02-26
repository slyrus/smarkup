
(in-package #:smarkup-test)

(defun test-xhtml-file ()
  (let ((smarkup-file
         (asdf:component-pathname
          (reduce #'asdf:find-component
                  '("smarkup-test" "test" "sample-sexp"))))
        (xhtml-file (merge-pathnames #p"sample.xhtml"
                                     (asdf:component-pathname
                                      (reduce #'asdf:find-component
                                              '("smarkup-test" "test"))))))
    (with-open-file (stream smarkup-file)
      (let ((sexp (read stream)))
        (let ((filtered (smarkup::apply-filters
                         sexp
                         '(:lisp :smarkup-metadata :html-metadata :ref))))
          (smarkup::render-as :xhtml filtered xhtml-file))))))
