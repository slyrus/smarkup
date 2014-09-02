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
(defclass smarkup-object-from-file (object-from-file)
  ((package :initarg :package :accessor object-package :initform nil)))

(defmethod perform :around ((o load-op)
                            (c smarkup-object-from-file))
  (let ((*readtable* (named-readtables:find-readtable 'quote-reader)))
    (let* ((p (object-package c))
           (*package* (or (when p (find-package p))
                          *package*)))
      (call-next-method))))

;;;
(defclass filtered-object (object-from-variable)
  ((filters :accessor object-filters :initarg :filters)))

(defmethod perform ((op compile-op) (c filtered-object))
  (call-next-method)
  (setf (symbol-value (object-symbol c))
        (apply-filters
         (symbol-value (object-symbol c))
         (object-filters c))))

(defmethod component-relative-pathname ((component filtered-object)))

(defmethod component-pathname ((component filtered-object)))

(defclass object-xhtml-file (object-from-variable source-file) ())

(defmethod perform ((op compile-op) (c object-xhtml-file))
  (call-next-method)
  (let ((sexp (symbol-value (object-symbol c)))
        (file (component-pathname c)))
    (render-as :xhtml sexp file)))

(defmethod perform ((op load-op) (c object-xhtml-file))
  (call-next-method)
  (open-in-web-browser (namestring (component-pathname c))))


(defclass pdf-file (generated-file) ())
(defmethod source-file-type ((c pdf-file) (s module)) "pdf")

(defmethod perform ((operation compile-op) (c pdf-file)))

(defparameter *pdf-viewer*
  #+linux "kpdf"
  #+darwin "/Applications/Preview.app"
  #-(or linux darwin) nil)

(defun pdf-open (&rest args)
  (when *pdf-viewer*
    #+darwin
    (apply #'app-open "-a" *pdf-viewer* (mapcar #'namestring args))
    #-darwin
    (run-program-asynchronously *pdf-viewer* 
                                (mapcar #'(lambda (x)
                                            (if (pathnamep x) (namestring x) x)) args))))

(defmethod perform ((operation load-op) (c pdf-file))
  (pdf-open (namestring (component-pathname c))))

(defmethod operation-done-p ((o load-op) (c pdf-file))
  nil)


(defclass object-cl-pdf-file (object-from-variable pdf-file) ())

(defmethod perform ((op compile-op) (c object-cl-pdf-file))
  (let* ((dir (asdf:component-pathname
               (asdf:component-system c)))
         (*default-pathname-defaults* dir))
    (call-next-method)
    (let ((sexp (symbol-value (object-symbol c)))
          (file (component-pathname c)))
      (render-as :cl-pdf sexp file))))

(defmethod perform ((op load-op) (c object-cl-pdf-file))
  (call-next-method)
  (app-open (namestring (component-pathname c))))


;;; css files

(defclass css-file (static-file) ())
(defmethod source-file-type ((c css-file) (s module)) "css")

;;; xhtml files

(defclass xhtml-file (html-file) ())
(defmethod source-file-type ((c xhtml-file) (s module)) "xhtml")

;;; tiff files

(defclass tiff-file (static-file) ())
(defmethod source-file-type ((c tiff-file) (s module)) "tiff")

;;; jpeg files

(defclass jpeg-file (static-file) ())
(defmethod source-file-type ((c jpeg-file) (s module)) "jpg")

;;; png files

(defclass png-file (static-file) ())
(defmethod source-file-type ((c png-file) (s module)) "png")

