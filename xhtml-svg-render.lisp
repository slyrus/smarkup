
(in-package :smarkup)

(defun svg-xml-node (svg-image)
  (car (stp:list-children (cxml:parse svg-image (stp:make-builder)))))

(defun serialize-element (element stream)
  (cxml-stp:serialize element stream)
  (runes:close-ystream (cxml::sink-ystream stream)))

(defun serialize-element-to-string (element)
  (let ((s (cxml:make-string-sink)))
    (serialize-element element s)))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :svg-image)) attrs body)
  (princ (serialize-element-to-string (svg-xml-node (car body))) *stream*))

(defmethod process-element ((document-type (eql :xhtml)) (tag (eql :bibliography)) attrs body)
  (call-next-method :xhtml
                    :list
                    nil
                    (loop for cite in (get-cite-keys)
                       collect
                       `(:item ,(citation-string cite)))))

