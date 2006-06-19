;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: smarkup-asdf.cl
;;; author: cyrus harmon
;;;

;;; miscellaneous functions

(in-package #:smarkup)

(defgeneric render-as (type sexp file))

(defun remove-from-plist (plist key)
  (loop for (x y) on plist by #'cddr
     append (unless (eql x key)
               (list x y))))

(defun find-file-for-types (default-file types)
  (loop for type in types
     do (let ((path (merge-pathnames (make-pathname :type type) default-file)))
          (when (probe-file path)
            (return path)))))

(defparameter *images-per-line* 5)

(defun multi-line-figure (image-sequence
                          caption
                          &key
                          (start 0)
                          (end (length image-sequence))
                          (images-per-line *images-per-line*)
                          (width "1in"))
  `(:figure
    ,@(loop for i from start below end by images-per-line
         collect
           `(:subfigure
             ,@(loop
                  for j from i below (min (+ i *images-per-line*) end)
                  collect
                    (let ((img (elt image-sequence j)))
                      `(:image ,(namestring img)
                               :width ,width)))))
    (:caption ,caption)))


