;;
;; Copyright (c) 2006, Cyrus Harmon
;;
;; filter.lisp - filters are used to process markup document
;; sexps. The intent is that these filters are independent from the
;; final rendering. Think of this as a set of preprocessing stages
;; after document parsing. So now we have a markup document which gets
;; parsed, filtered and then rendered.
;;
;; Yes, perhaps the name "filter" could be better. I'm open to
;; suggestions.

(in-package :smarkup)

(defgeneric filter-gf (filter car list))
(defgeneric filter (type sexp))

(defmethod filter-gf (filter car list)
  (cond ((null car) (when (cdr list) (filter filter (cdr list))))
        ((atom car) (cons car (filter filter (cdr list))))
        (t (cons (filter filter (car list))
                 (filter filter (cdr list))))))

(defmethod filter (type sexp)
  (filter-gf type (car sexp) sexp))

(defun apply-filters (sexp filters)
  (cond ((null filters) sexp)
        ((listp filters) (apply-filters (filter (car filters) sexp) (cdr filters)))
        (t (filter filters sexp))))

;;;
;;; test filter. can reverse strings
;;;
(defun reverse-strings (sexp)
  (cond ((null sexp) nil)
        ((atom sexp) (if (stringp sexp)
                         (reverse sexp)
                         sexp))
        (t (cons (reverse-strings (car sexp))
                 (reverse-strings (cdr sexp))))))

(defmethod filter-gf ((filter (eql :test)) (car (eql :reverse)) list)
  `(:span ,@(filter :test (reverse-strings (reverse (cdr list))))))

;;;
;;; lisp filter. can eval lisp forms
;;;
(defun collect-string (str-list)
  (declare (optimize (debug 2)))
  (apply #'concatenate 'string
         (mapcar
          #'(lambda (x)
              (unless (equal x 'blank-line)
                (format nil "~A" x)))
          str-list)))

(defun lc-format (dest ctrl-string &rest args)
  (let ((*print-case* :downcase))
    (apply #'format dest ctrl-string args)))

(defun eval-lisp (tag body &key (show-commands t) (show-results t) (hunk t))
  (declare (ignore tag))
  (let ((lines
         (with-input-from-string (ifs (collect-string body))
           (loop for line = (read ifs nil)
              while line collect line))))
    (if (not (or show-commands show-results))
        (progn (mapcar #'eval lines)
               nil)
        (if hunk
            `((:div :class "lisp")
              (:div :class "lisp-code"
                    (:code 
                     (:pre ,(when show-commands
                                  (lc-format nil "~{~W~^~%~%~}" lines)))))
              ,@(progn
                 (let ((output (eval `(progn ,@lines))))
                   (if show-results
                       (list `(:div :class "lisp-results"
                                    (:pre (:results
                                           ,(if (stringp output)
                                                (lc-format nil "~W" output)
                                                (format nil "~S" output))))))
                      (list #\Newline)))))
            `((:div :class "lisp")
              ,@(mapcan #'(lambda (x)
                            (cons `(:code 
                                    (:pre ,(when show-commands
                                                 (lc-format nil "~W" x))))
                                  (let ((output (eval x)))
                                    (if show-results
                                        (list `(:div :class "lisp-results"
                                                     (:pre
                                                      (:results
                                                       ,(if (stringp output)
                                                            (lc-format nil "~W" output)
                                                            (format nil "~S" output)))))
                                              #\Newline)
                                        (list #\Newline)))))
                        lines))))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp)) list)
  (eval-lisp car (cdr list)))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-no-results)) list)
  (eval-lisp car (cdr list) :show-results nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-no-commands)) list)
  (eval-lisp car (cdr list) :show-commands nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-silent)) list)
  (eval-lisp car (cdr list) :show-results nil :show-commands nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-no-eval)) list)
  `((:div :class "lisp")
    (:div :class "lisp-code"
          (:code
           (:pre ,@(cdr list))))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-value)) list)
  (eval (read-from-string (car (cdr list)))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :setf-lisp-value)) list)
  (let ((sexp (read-from-string (car (cdr list)))))
    (setf (symbol-value (car sexp)) (cadr sexp))
    nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :code-block)) list)
  `((:div :class "lisp") (:pre ,@(cdr list))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :document-class)) list)
  (let ((class (cadr list)))
    `((:div :class "doc class")
      (:p :class "doc-type" "[CLASS]")
      (:p :class "doc-name class-name" ,(symbol-name class))
      (:p :class "doc-documentation class-documentation" ,(documentation class 'type)))))

#+sbcl
(defmethod filter-gf ((filter (eql :lisp)) (car (eql :generic-function)) list)
  (let ((gf (cadr list)))
    `((:div :class "doc generic-function")
      (:p :class "doc-type" "[GENERIC FUNCTION]") 
      (:p :class "doc-name gf-name" "("
          ,@(let ((fname (sb-mop:generic-function-name (fdefinition gf)))
                  (lambda-list (sb-mop:generic-function-lambda-list (fdefinition gf))))
                 (if (listp fname)
                     (append
                      (list (symbol-name (car fname))
                             " ("
                             (symbol-name (cadr fname))
                             " "
                             (butlast
                              (mapcan (lambda (x)
                                        (list (symbol-name x) " "))
                                      (cdr lambda-list)))
                             ") "
                             (symbol-name (car lambda-list))))
                     (butlast
                      (mapcan (lambda (x)
                                (list (symbol-name x) " "))
                              (cons fname lambda-list)))))
          ")")
      (:p :class "doc-documentation gf-documentation" ,(documentation gf 'function)))))

#+sbcl
(defmethod filter-gf ((filter (eql :lisp)) (car (eql :function)) list)
  (declare (optimize (debug 3)))
  (let ((fun (cadr list)))
    `((:div :class "doc function")
      (:p :class "doc-type" "[FUNCTION]") 
      (:p :class "doc-name fn-name" "("
          ,@(let ((fname (nth-value 2 (function-lambda-expression (fdefinition fun))))
                  (lambda-list (sb-introspect:function-lambda-list (fdefinition fun))))
                 (if (listp fname)
                     (append
                      (list (symbol-name (car fname))
                             " ("
                             (symbol-name (cadr fname))
                             " "
                             (butlast
                              (mapcan (lambda (x)
                                        (list (symbol-name x) " "))
                                      (cdr lambda-list)))
                             ") "
                             (symbol-name (car lambda-list))))
                     (butlast
                      (mapcan (lambda (x)
                                (list (symbol-name x) " "))
                              (cons fname lambda-list)))))
          ")")
      (:p :class "doc-documentation fn-documentation" ,(documentation fun 'function)))))

;;;
;;; markup-metadata filter. sets various special variables with
;;; document metadata info
;;;
(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :copyright)) list)
  (setf *copyright* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :title)) list)
  (setf *document-title* (cdr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :titlerunning)) list)
  (setf *document-titlerunning* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :subtitle)) list)
  (setf *document-subtitle* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :author)) list)
  (setf *document-author* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :tocauthor)) list)
  (setf *document-tocauthor* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :authorrunning)) list)
  (setf *document-authorrunning* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :address)) list)
  (setf *document-address* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :institute)) list)
  (setf *document-institute* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :bibtex-database)) list)
  (setf *bibtex-database* (make-hash-table :test #'equalp))
  (setf *cite-keys* nil)
  (setf *cite-order* (make-hash-table :test #'equalp))
  (setf *current-citation* 0)
  (let ((database-spec-list (cadr list)))
    (loop for database in database-spec-list
       do
       (let ((bibtex-runtime::*bib-database* *bibtex-database*)
             (bibtex-runtime::*bib-macros* *bibtex-macros*))
         (with-open-file (f database)
           (bibtex-runtime:read-bib-database f)))))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata))
                      (car (eql :bibtex-style))
                      list)
  (let ((bst (cadr list)))
    (setf *bibtex-style* bst)))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :smarkup-metadata)) list)
  (filter-gf filter (cadr list) (cdr list))
  nil)

(defparameter *html-css-stylesheet-url* nil)

(defmethod filter-gf ((filter (eql :html-metadata)) (car (eql :htmlcss)) list)
  (pushnew (cdr list) *html-css-stylesheet-url* :test 'equal)
  (call-next-method))

(defmethod filter-gf ((filter (eql :html-metadata)) (car (eql :html-metadata)) list)
  (setf *html-css-stylesheet-url* nil)
  (filter-gf filter (cadr list) (cdr list))
  nil)



;;;
;;; references
;;;


;;;
;;; Ok, need to get the bibtex-style and then call bibtex or its guts to get the references.
;;;

(defun get-bib-entry (entry)
  (with-output-to-string (stream)
    (bibtex-runtime::write-bib-entry entry stream)))

(defun get-bib-order (entry)
  (gethash entry *cite-order*))

(defmethod filter-gf ((filter (eql :ref)) (car (eql :bibcite)) list)
  (loop for  cite-key in (cdr list)
     do
       (unless (member cite-key *cite-keys* :test 'string=)
         (push cite-key *cite-keys*)
         (setf (gethash cite-key *cite-order*) (incf *current-citation*)))
       (let ((v (gethash cite-key *bibtex-database*)))
         (if v
             (bibtex-runtime::write-bib-entry v)
             (warn "~%bibliography entry ~A not found~%" cite-key))))
  (call-next-method))


;;;
;;; outline stuff
;;;

(defparameter *outline-elements* '(:h1 :h2 :h3 :h4 :h5 :h6))

(defparameter *outline-level* 6)

(defmethod filter-gf ((filter (eql :outline)) car list)
  (let ((outline-elements
         (subseq *outline-elements*
                 0 (min (length *outline-elements*)
                        *outline-level*))))
    (remove-if-not #'(lambda (x) (member (car x) outline-elements))
                   list)))

