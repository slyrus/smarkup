;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: smarkup-asdf.cl
;;; author: cyrus harmon
;;;

;;; miscellaneous functions

(in-package #:smarkup)

;;; characters

(defparameter *no-break-space*
  #+sbcl #\No-Break_Space
  #+abcl #\u00A0
  #-(or sbcl abcl) nil)

(defparameter *left-double-quotation-mark*
  #+sbcl #\LEFT_DOUBLE_QUOTATION_MARK
  #+abcl #\u201c
  #-(or sbcl abcl) nil)

(defparameter *right-double-quotation-mark*
  #+sbcl #\RIGHT_DOUBLE_QUOTATION_MARK
  #+abcl #\u201d
  #-(or sbcl abcl) nil)

(defparameter *em-dash*
  #+sbcl #\EM_DASH
  #+abcl #\u2014
  #-(or sbcl abcl) nil)

;;; utilities

(defun keywordicate (x)
  (cond ((keywordp x) x)
        (t (intern (string-upcase x) 'keyword))))

(defun remove-keywordish-args (keywords &rest args)
  (let ((keys))
    (let ((non-keys
           (loop for (x y) on args
              with skip = nil
              append (if skip
                         (setf skip nil)
                         (if (member x keywords)
                             (progn
                               (setf skip t)
                               (pushnew (cons x y) keys)
                               nil)
                             (list x))))))
      (list (mapcan #'(lambda (x)
                        (list (car x) (cdr x)))
                    (nreverse keys))
            non-keys))))

(defun keyword-arg-name (key)
  (cond ((atom key)  key)
        ((listp key) (car key))))

(defmacro with-keyword-args (((&rest args) rest) list &body body)
  `(destructuring-bind ((&key ,@args) (&rest ,rest))
       (apply #'remove-keywordish-args
              (mapcar #'keywordicate
                      (mapcar #'keyword-arg-name
                              ',args))
              ,list)
     ,@body))

(defgeneric render-as (type sexp file))

(defun remove-from-plist (plist &rest keys)
  (cond ((eql (length keys) 1)
         (loop for (x y) on plist by #'cddr
            append (unless (eql x (car keys))
                     (list x y))))
        ((> (length keys) 1)
         (reduce (lambda (&optional plist x)
                   (when x (remove-from-plist plist x)))
                 (cons plist keys)))))

(defun remove-pair-from-list (list key)
  (let ((pos (position key list)))
    (if pos
        (append (subseq list 0 pos)
                (subseq list (+ pos 2))))))

(defun find-file-for-types (default-file types)
  (loop for type in types
     do (let ((path (merge-pathnames (make-pathname :type type) default-file)))
          (when (probe-file path)
            (return path)))))

(defparameter *images-per-line* 5)
(defparameter *images-per-page* 30)

(defun multi-line-figure (image-sequence
                          caption
                          &key
                          label
                          (start 0)
                          (end)
                          (images-per-line *images-per-line*)
                          (width "1.1in"))
  (let* ((image-sequence
          (mapcan #'(lambda (x)
                      (when x (list x)))
                  image-sequence))
         (end (or end (1- (length image-sequence)))))
    (when (some #'identity image-sequence)
      `(:figure
        ,@(when label `(:label ,label))
        (:centering
         ,@(loop for i from start to end by images-per-line
              collect
              `(:subfigure
                ,@(loop
                     for j from i to (min (+ i images-per-line -1) end)
                     collect
                     (let ((img (elt image-sequence j)))
                       `(:image ,(namestring img)
                                :width ,width))))))
        ,(when caption `(:caption ,@(if (listp caption) caption (list caption))))))))


(defun multi-multi-line-figure (image-sequence
                                &key
                                caption
                                (first-caption caption)
                                (start 0)
                                (end)
                                (images-per-line *images-per-line*)
                                (images-per-page *images-per-page*)
                                (width "1in"))
  (let* ((image-sequence
          (mapcan #'(lambda (x)
                      (when x (list x)))
                  image-sequence))
         (end (or end (1- (length image-sequence)))))
    `(:span
      ,@(loop for i from start to end by images-per-page
           collect
           (multi-line-figure image-sequence (if (= i start)
                                                 first-caption
                                                 caption)
                              :start i :end (min (+ i images-per-page -1) end)
                              :images-per-line images-per-line
                              :width width)))))

(defun multi-line-subfigure (image-sequence
                             &key
                             caption
                             (start 0)
                             (end (1- (length image-sequence)))
                             (images-per-line *images-per-line*)
                             increment-counter
                             (width "1in"))
  (when (some #'identity image-sequence)
    (append
     (loop for i from start to end by images-per-line
        collect
          (cons
           :subfigure
           (append
            (loop
               for j from i to (min (+ i images-per-line -1) end)
               collect
               (let ((img (elt image-sequence j)))
                 `(:image ,(namestring img)
                          :width ,width)))
            (if (and caption (> (+ i images-per-line) end))
                `(:caption ,caption)
                (unless increment-counter
                  `(:increment-counter nil)))))))))

;;; smarkup document parsing routines
;;; the general structure of an smarkup document is:
;;;
;;; document ::= element*
;;;
;;; element ::= atom | tagged-element
;;;
;;; tagged-element ::= (tag body) | (tag attrs body) | ((tag attrs) body)
;;;
;;; body ::= element*
;;;
;;; tag ::= keyword
;;;
;;; attrs ::= attr*
;;;
;;; attr ::= attr-name attr-value
;;;
;;; attr-name ::= keyword
;;;

(defun process-body (document-type body)
  (loop for element in body
     do (parse-element document-type element)))

(defgeneric process-element (document-type tag attrs body))

(defmethod process-element (document-type tag attrs body)
  (process-body document-type body))

(defun parse-tag-and-attribute-list (list)
  "Takes a list of the form (tag :attr1 value1 :attr2 value2) and
returns the two values tag and ((:attr1 . value1) (:attr2
  . value2))."
  (values (car list)
          (loop for (name value) on (cdr list) by #'cddr
             collect (cons name value))))

(defun parse-tag-attributes-and-body (list)
  "Takes a list of the form (tag :attr1 value1 ... :attrn valuen body)
and returns three values: tag, ((:attr1 . value1) ... (:attr2
. value2)), and body. There may be no attributes specified. Attributes
are distinguished by keywords. The first non-keyword value in a
possible attribute name position and all subsequent items are treated
as the body."
  (when (listp list)
    (loop for (name value) on (cdr list) by #'cddr
       with attrs
       with body
       with parsing-attrs = t
       do (cond ((and parsing-attrs (keywordp name))
                 (push (cons name value) attrs))
                (t
                 (push name body)
                 (when value (push value body))))
       finally (return (values (car list)
                               (nreverse attrs)
                               (nreverse body))))))

(defun parse-element (document-type element)
  "Parse an smarkup element and return the content of the
  element. [tagged-element ::= (tag body) | (tag attrs body) | ((tag
  attrs) body)]"
  (cond ((atom element)
         (process-element document-type element nil nil))
        (t (cond ((listp (car element))
                  (multiple-value-bind (tag attrs)
                      (parse-tag-and-attribute-list (car element))
                    (process-element document-type tag attrs (cdr element))))
                 (t
                  (multiple-value-bind (tag attrs body)
                      (parse-tag-attributes-and-body element)
                    (process-element document-type tag attrs body)))))))

(defun parse-document (document-type doc)
  "Parse an smarkup document and return the elements contained in the
the document. [document ::= element*]."
  (loop for element in doc
     do (parse-element document-type element)))
