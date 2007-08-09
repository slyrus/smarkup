
(in-package :smarkup)

(eval-when (:compile-toplevel)
  #.(enable-quote-reader-macro))

(defparameter *xhtml-header*
  #q{<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html 
     PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">})

#+nil
`((:bibcite      . html-citation)
  (:bibliography . html-bibliography)
  (:image        . htmlize-image)
  (:url          . htmlize-url)
  (:href         . htmlize-href)
  (:h1           . htmlize-header)
  (:h2           . htmlize-header)
  (:h3           . htmlize-header)
  (:h4           . htmlize-header))

(defparameter *html-element-transformation*
  `((:example      . :pre)
    (:sidebarhead  . (:div :class "sidebarhead"))
    (:sidebar      . (:div :class "sidebar"))
    (:note         . (:div :class "note"))
    (:note-ref     . :sup)
    (:bullets      . :ul)
    (:list         . :ul)
    (:item         . :li)
    (:results      . (:code :class "results"))
    (:figure       . (:table :class "figure"))
    (:figure*       . (:table :class "figure"))
    (:subfigure    . (:div :class "subfigure"))))

(defparameter *stream* nil)

;;; TODO! Fix html-citation and add the other html tag filter functions!


(defun html-citation (tag body)
  (declare (ignore tag))
  `(:span
    ,@(loop for ref in body
         collect
         (let ((refnum (get-bib-order ref)))
           `((:a :href
                 ,(format nil "#ref~A" refnum))
             ,(format nil "[~A]" refnum))))))

(defun citation-string (cite)
  (let ((refnum (get-bib-order cite)))
    `((:a :name
          ,(format nil "ref~A" refnum))
      ,(format nil "~A. ~A"
               refnum
               (cite-text cite)))))

(defun html-bibliography (tag body)
  (declare (ignore tag body))
  `(:div
    (:h1 "References")
    ,@(loop for cite in (reverse *cite-keys*)
         collect `(:p ,(citation-string cite)))))
    
(defun htmlize-image (tag body)
  (declare (ignore tag))
  (destructuring-bind (src &key width types) body
    (declare (ignore width))
    (when types
      (let ((src2 (find-file-for-types src types)))
        (when src2 (setf src (enough-namestring src2)))))
    `((:img :src ,src))))

(defun htmlize-url (tag body)
  (declare (ignore tag))
  `(:a :href ,@body ,@body))

(defun htmlize-href (tag body)
  (declare (ignore tag))
  (destructuring-bind ((hreftag href) (texttag text)) body
    (declare (ignore hreftag texttag))
    `(:a :href ,href ,text)))

(defun htmlize-header (tag body)
  (destructuring-bind (heading &rest rest &key clearpage no-number &allow-other-keys) body
    (declare (ignore clearpage no-number))
    `(,tag ,@(cons heading (remove-from-plist rest :clearpage :no-number)))))

(defun transform-sexp (sexp)
  (if (and (listp sexp) (car sexp))
      (let ((xfrm (cdr (assoc (car sexp) *html-element-transformation*))))
        (cond ((null xfrm) sexp)
              ((and (symbolp xfrm) (fboundp xfrm))
               (funcall (symbol-function xfrm) (car sexp) (cdr sexp)))
              (t (setf (car sexp) xfrm)
                 sexp)))
      sexp))

  
;;;
;;; * input is a list of S-expressions, of the form:
;;;   (tag &rest content)
;;; * tag can be either a keyword (like :p)
;;;   or a list (like (:a :href "http://myurl.com"))

(defun keyword-to-string (keyword)
  (string-downcase (symbol-name keyword)))

(defun attributes-to-list (attr)
  (loop for (attr val) on attr by #'cddr
     collect (list (if (keywordp attr)
                       (keyword-to-string attr)
                       attr)
                   val)))

;;; return multiple-values tag, list of attrs
(defun parse-tag (tag)
  (let ((tag (transform-sexp tag)))
    (cond ((atom tag)
           (when (keywordp tag)
             (keyword-to-string tag)))
;;; need to check if (car tag) is a list. Handle three cases, atom, (list ...), ((list) list)
          ((and (listp tag) (atom (car tag)))
           (values (parse-tag (car tag))
                   (attributes-to-list (cdr tag))))
          (t (values (parse-tag (car tag))
                     (attributes-to-list (cdr tag)))))))



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *xml-char-map*
    (ch-util::make-hash-table-from-alist
     '((#\< . "&lt;")
       (#\> . "&gt;")
       (#\& . "&amp;")
       (#\No-Break_Space . "&#xa0;")
       (#\LEFT_DOUBLE_QUOTATION_MARK . "&#8220;")
       (#\RIGHT_DOUBLE_QUOTATION_MARK . "&#8221;")
       (#\EM_DASH . "&#8212;")
       (:eol . #\Space))))

  (defun get-xml-char (c)
    (let ((xc (gethash c *xml-char-map*)))
      (if xc xc c))))

(defun render-text (out text)
  (declare (optimize (debug 3)))
  (macrolet ((match-second-char (c1 c2 out-char)
                `(let ((n (peek-char nil in nil nil)))
                  (if (eql n ,c2)
                      (progn
                        (read-char in)
                        (princ ,(get-xml-char out-char) out))
                      (princ ,(get-xml-char c1) out)))))
    (cond ((stringp text)
           (with-input-from-string (in text)
             (loop for c = (read-char in nil nil) while c
                do 
                (cond ((eql c #\~)
                       (princ (get-xml-char #\No-Break_Space) out))
                      ((eql c #\.)
                       (match-second-char #\. #\\ #\Space))
                      ((eql c #\`)
                       (match-second-char #\` #\` #\LEFT_DOUBLE_QUOTATION_MARK))
                      ((eql c #\')
                       (match-second-char #\' #\' #\RIGHT_DOUBLE_QUOTATION_MARK))
                      ((eql c #\-)
                       (match-second-char #\- #\- #\EM_DASH))
                      (t (princ (get-xml-char c) out))))))
          (t (princ (get-xml-char text) out)))))

(defun render-content (content)
  (loop for s in content
     do (parse-element :xhtml s)))

(defparameter *indent-level* 0)

(defun render-element-tag (stream tag attributes)
  (format stream "<~A~{~^ ~{~A=~S~^ ~}~}/>" (string-downcase tag)
          (mapcar (lambda (x)
                    (list (car x)
                          (cdr x)))
                  attributes)))

(defun render-element-open-tag (stream tag attributes)
  (format stream "<~A~{~^ ~{~A=~S~^ ~}~}>" (string-downcase tag)
          (mapcar (lambda (x)
                    (list (car x)
                          (cdr x)))
                  attributes))
  (incf *indent-level*))

(defun render-element-close-tag (stream tag)
  (format stream "</~a>~&" (string-downcase tag))
  (decf *indent-level*))

#+nil
(defun render-sexp (stream sexp)
  (declare (optimize (debug 3)))
  (cond ((null sexp) nil)
        ((atom sexp)
         )
        (t
         (let ((xfrm-sexp (transform-sexp sexp)))
           (render-element stream (car xfrm-sexp) (cdr xfrm-sexp))))))

(defmethod process-element ((document-type (eql :xhtml)) (tag string) attrs body)
  (render-text *stream* tag))

(defmethod process-element ((document-type (eql :xhtml)) tag attrs body)

  (let ((xfrm (cdr (assoc tag *html-element-transformation*))))
    (when xfrm
      (cond ((atom xfrm)
             (setf tag xfrm))
            (t
             (setf tag (car xfrm))
             (setf attrs (append (loop for (x y)
                                    on (cdr xfrm)
                                    collect (cons x y))
                                 attrs))))))
  
  (when tag
    (if body
        (progn
          (render-element-open-tag *stream* tag attrs)
          (call-next-method)
          (render-element-close-tag *stream* tag))
        (render-element-tag *stream* tag attrs))))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :eol)) attrs body)
  (process-element :xhtml :br attrs body))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :h1)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :h2)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :h3)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :h4)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :p)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :b)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :code)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :pre)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :item)) attrs body)
  (call-next-method :xhtml
                    :li attrs body))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :bibcite)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :image)) attrs body)
  (call-next-method))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :bibliography)) attrs body)
  (call-next-method))

(defmethod render-as ((type (eql :xhtml)) sexp file)
  (let ((*document-render-type* :xhtml))
      (setf *indent-level* 0)
    (with-open-file (out file
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (write-sequence *xhtml-header* out)
      (let ((*stream* out))
        (parse-document :xhtml
                        `(((:html :|xmlns| "http://www.w3.org/1999/xhtml" "xml:lang" "en" :lang "en")
                           (:head
                            ,(when *document-title*
                                   `(:title ,@*document-title*))
                            ,(when *html-css-stylesheet-url*
                                   `((:link :rel "stylesheet" :type "text/css"
                                            :href ,*html-css-stylesheet-url*))))
                           (:body ,@sexp))))))))

(defun render-sexp-to-string (sexp)
  (with-output-to-string (string)
    (let ((*stream* string))
      (parse-document :xhtml sexp))))

(defun render-string (str)
  (with-output-to-string (string)
    (smarkup::render-text string str)))