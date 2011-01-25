;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: smarkup-asdf.cl
;;; author: cyrus harmon
;;;

;;;
;;; smarkup ASDF classes and methods for various component types
;;; and operations on them

(in-package :smarkup)

(defmacro run-program (&rest args)
  #+sbcl `(sb-ext:run-program ,@args))

(defun app-open (&rest args)
  #+darwin (run-program "/usr/bin/open"
                        (mapcar #'(lambda (x) (if (pathnamep x) (namestring x) x))
                                args)))

(defun open-in-web-browser (&rest args)
  #+darwin
  (apply #'app-open (list* "-a" "/Applications/Safari.app"
                           (mapcar #'(lambda (x) (if (pathnamep x) (namestring x) x)) args))))

(defmacro with-current-directory (dir &body body)
  `(unwind-protect (progn
		     #+sbcl
                     (sb-posix:chdir ,dir)
                     (let ((*default-pathname-defaults* ,dir))
                       ,@body))
     #+sbcl (sb-posix:chdir *default-pathname-defaults*)))

;;;
(defclass smarkup-object-from-file (ch-asdf:object-from-file) ())

(defmethod perform :around ((o load-op)
                            (c smarkup-object-from-file))
  (let ((*readtable* (named-readtables:find-readtable 'quote-reader)))
    (call-next-method)))

;;;
(defclass filtered-object (ch-asdf:object-from-variable)
  ((filters :accessor object-filters :initarg :filters)))

(defmethod perform ((op compile-op) (c filtered-object))
  (call-next-method)
  (setf (symbol-value (ch-asdf:object-symbol c))
        (apply-filters
         (symbol-value (ch-asdf:object-symbol c))
         (object-filters c))))

(defmethod component-relative-pathname ((component filtered-object)))

(defmethod component-pathname ((component filtered-object)))

(defparameter *pdflatex-program* "pdflatex")

(defclass object-latex-file (ch-asdf:object-from-variable generated-file) ())

(defmethod perform ((op ch-asdf:generate-op) (c object-latex-file))
  (call-next-method)
  (render-as :latex
             (symbol-value (ch-asdf:object-symbol c))
             (component-pathname c)))

(defmacro with-component-directory ((component) &body body)
  `(with-current-directory
       (make-pathname
        :directory (pathname-directory
                    (component-pathname ,component)))
     ,@body))

(defmethod perform ((operation compile-op) (c object-latex-file))
  (with-component-directory (c)
    (let ((unix-path (namestring (component-pathname c))))
      (run-program *pdflatex-program*
                            (list unix-path))
      ;; we have to do this twice to get the references right!
      ;; maybe 3x?
      (run-program *pdflatex-program*
                            (list unix-path)))))

(defmethod operation-done-p ((o ch-asdf:generate-op) (c object-latex-file))
  (let ((on-disk-time
         (file-write-date (component-pathname c)))
        (obj (asdf:find-component
              (asdf:component-parent c)
              (asdf:coerce-name (ch-asdf:object-input-object c)))))
    
    (let ((obj-date (asdf:component-property obj 'ch-asdf:last-loaded)))
      (and on-disk-time
           obj-date
           (>= on-disk-time obj-date)))))

(defclass object-xhtml-file (ch-asdf:object-from-variable source-file) ())

(defmethod perform ((op compile-op) (c object-xhtml-file))
  (call-next-method)
  (let ((sexp (symbol-value (ch-asdf:object-symbol c)))
        (file (component-pathname c)))
    (render-as :xhtml sexp file)))

(defmethod perform ((op load-op) (c object-xhtml-file))
  (call-next-method)
  (open-in-web-browser (namestring (component-pathname c))))

(defclass object-cl-pdf-file (ch-asdf:object-from-variable pdf-file) ())

(defmethod perform ((op compile-op) (c object-cl-pdf-file))
  (call-next-method)
  (let ((sexp (symbol-value (ch-asdf:object-symbol c)))
        (file (component-pathname c)))
    (render-as :cl-pdf sexp file)))

(defmethod perform ((op load-op) (c object-cl-pdf-file))
  (call-next-method)
  (app-open (namestring (component-pathname c))))

