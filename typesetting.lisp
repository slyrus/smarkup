;;
;; Copyright (c) 2007, Cyrus Harmon
;;
;; typesetting.cl - cl-typesettng/cl-pdf output for smarkup
;;

(in-package :tt)

(defmacro with-paragraph (style-designator &body body)
  (with-gensyms (top-margin bottom-margin first-line-indent
                            style new-style restore-style first-indent)
    `(let* ((,style ,style-designator)
            (,top-margin (getf ,style :top-margin 0))
            (,bottom-margin (getf ,style :bottom-margin 0))
            (,first-line-indent (getf ,style :first-line-indent 0))
            (,new-style (typecase ,style
                          (text-style ,style)
                          (t (apply #'make-instance 'text-style ,style))))
            (,restore-style (make-restore-style ,new-style))
            (,first-indent ,first-line-indent))
       (add-box ,new-style)
       (use-style ,new-style)
       (add-box (make-instance 'v-spacing :dy ,top-margin))
       (unless (zerop ,first-indent)
         (add-box (make-instance 'h-spacing :dx ,first-indent)))
       ,@(mapcar 'insert-stuff body)
       (unless (eq (first (boxes-tail *content*)) :eol)
         (add-box :eol))
       (add-box (make-instance 'v-spacing :dy ,bottom-margin))
       (add-box ,restore-style)
       (use-style ,restore-style))))

(defmacro with-dynamic-style (style &body body)
  (with-gensyms (new-style restore-style)
    `(let* ((,new-style (apply #'make-instance 'text-style ,style))
	    (,restore-style (make-restore-style ,new-style)))
      (add-box ,new-style)
      (use-style ,new-style)
      ,@(mapcar 'insert-stuff body)
      (add-box ,restore-style)
      (use-style ,restore-style))))

(in-package :smarkup)

(defvar *verbatim* nil)

(defun put-smarkup-string (string)
  (if *verbatim*
      (tt:verbatim string)
      (tt:put-string
       (macrolet ((match-second-char (c1 c2 out-char)
                    `(let ((n (peek-char nil in nil nil)))
                       (if (eql n ,c2)
                           (progn
                             (read-char in)
                             (write-char ,out-char out))
                           (write-char ,c1 out)))))
         (with-output-to-string (out)
           (with-input-from-string (in string)
             (loop for c = (read-char in nil nil) while c
                do 
                (cond ((eql c #\~)
                       (write-char *no-break-space* out))
                      ((eql c #\.)
                       (match-second-char #\. #\\ #\Space))
                      ((eql c #\`)
                       (match-second-char #\` #\` (code-char #x93)))
                      ((eql c #\')
                       (match-second-char #\' #\' (code-char #x94)))
                      ((eql c #\-)
                       (match-second-char #\- #\- (code-char #x97)))
                      (t (write-char c out))))))))))

(defparameter *default-paragraph-style*
  '(:h-align :justified
    :font "Times-Roman"
    :font-size 10
    :first-line-indent 18
    :bottom-margin 3))

(defparameter *default-bold-style*
  '(:font "Times-Bold"
    :font-size 10))

(defparameter *default-code-style*
  '(:font "Courier"
    :font-size 9))

(defparameter *default-results-style*
  '(:font "Courier-Bold"
    :font-size 9))

(defparameter *default-pre-style*
  '(:font "Courier"
    :font-size 9
    :bottom-margin 3))

(defparameter *default-h1-style*
  '(:h-align :left :font "Helvetica-Bold" :font-size 14 :bottom-margin 3))

(defparameter *default-h2-style*
  '(:font "Helvetica-Bold" :font-size 11 :bottom-margin 3 :top-margin 3))

(defparameter *default-h3-style*
  '(:font "Helvetica-Bold" :font-size 10 :bottom-margin 3 :top-margin 3))

(defparameter *default-h4-style*
  '(:font "Helvetica-Bold" :font-size 10 :bottom-margin 3 :top-margin 3))

(defparameter *item-decorator*
  (format nil "~A " (code-char #x81)))

(defparameter *default-item-style*
  (concatenate
   'list
   '(:left-margin 18)
   *default-paragraph-style*))

(defun get-item-decorator-width ()
  (let ((font-size (or (getf *default-item-style* :font-size)
                       tt::*font-size*)))
    (let* ((item-font (getf *default-item-style* :font))
           (font (if item-font
                     (pdf:get-font item-font)
                     tt::*font*)))
      (loop for char across *item-decorator*
         summing (pdf:get-char-width char font font-size)))))

(setf (getf *default-item-style* :first-line-indent)
      (- (get-item-decorator-width)))

(defmethod process-element ((document-type (eql :pdf)) (tag string) attrs body)
  (put-smarkup-string tag))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :eol)) attrs body)
  (tt:new-line))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :h1)) attrs body)
  (tt::with-paragraph *default-h1-style*
    (call-next-method)))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :h2)) attrs body)
  (tt::with-paragraph *default-h2-style*
    (call-next-method)))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :h3)) attrs body)
  (tt::with-paragraph *default-h3-style*
    (call-next-method)))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :h4)) attrs body)
  (tt::with-paragraph *default-h4-style*
    (call-next-method)))


(defmethod process-element ((document-type (eql :pdf)) (tag (eql :p)) attrs body)
  (tt::with-paragraph *default-paragraph-style*
    (call-next-method)))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :b)) attrs body)
  (tt::with-dynamic-style *default-bold-style*
    (call-next-method)))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :code)) attrs body)
  (tt::with-dynamic-style *default-code-style*
    (let ((*verbatim* t))
      (call-next-method))))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :results)) attrs body)
  (tt::with-dynamic-style *default-results-style*
    (let ((*verbatim* t))
      (call-next-method))))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :pre)) attrs body)
  (tt::with-paragraph *default-pre-style*
    (let ((*verbatim* t))
      (call-next-method))))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :item)) attrs body)
  (tt::with-paragraph *default-item-style*
    (tt:put-string *item-decorator*)
    (call-next-method)))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :bibcite)) attrs body)
  (tt:put-string
   (format nil "[~{~A~^,~}]"
           (loop for ref in body
              collect (get-bib-order ref)))))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :image)) attrs body)
  (let* ((file (car body))
         (type (pathname-type file)))
    (let ((type-arg
           (cond
             ((equal type "png") :png))))
      (apply #'tt:image :file file :dx 100 :dy 100
             (when type-arg `(:image-type ,type-arg))))))

(defmethod process-element ((document-type (eql :pdf)) (tag (eql :bibliography)) attrs body)
  (process-element document-type :h1 nil '("References"))
  (loop for cite in (reverse *cite-keys*)
     do
       (let ((refnum (get-bib-order cite)))
         (process-element document-type :p nil
                          (list
                           (format nil "~A. ~A"
                                   refnum
                                   (cite-text cite)))))))


(defparameter *pdf-header-function* nil)
(defparameter *pdf-title-page-function* nil)

(defparameter *pdf-default-page-size* :letter)
(defparameter *pdf-default-margins* '(72 72 72 72))

(defun pdf-draw-pages (content)
  (apply #'tt:draw-pages
             content
             :size *pdf-default-page-size*
             :margins *pdf-default-margins*
             :break :before
             (when *pdf-header-function*
               `(:header ,(funcall *pdf-header-function*))))
  (when pdf:*page* (typeset:finalize-page pdf:*page*)))

(defmethod render-as ((type (eql :cl-pdf)) sexp file)
  (setq cl-typesetting-hyphen::*left-hyphen-minimum* 999
        cl-typesetting-hyphen::*right-hyphen-minimum* 999)
  (tt:with-document ()
    (when *pdf-title-page-function*
      (funcall *pdf-title-page-function*))
    
    (let ((content
           (tt::with-text-content ((make-instance 'tt::text-content) :dont-save-style t)
             (tt::add-box (tt::copy-style (tt::text-style tt::*content*)))
             (when *document-title*
               (process-element :pdf :h1 nil *document-title*))
             (parse-document :pdf sexp)
             tt::*content*)))
      (pdf-draw-pages content))
    (tt:write-document file)))
