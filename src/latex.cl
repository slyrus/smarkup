;;
;; Copyright (c) 2006, Cyrus Harmon
;;
;; latex.cl - latex output from smarkup
;;

(in-package :smarkup)

(defparameter *baseline-skip* "10pt")
(defparameter *baseline-stretch* "1.6")
(defparameter *par-skip* "18pt")
(defparameter *latex-graphics-params* nil)

(defun latex-command (command &optional arg)
  (format nil "~&\\~A~@[{~A}~]~%" command arg))

(defgeneric emit-latex (stream thing &key newline))
(defgeneric emit-latex-gf (stream type children &key newline))

;;; default is a no-op
(defmethod emit-latex (stream thing &key newline)
  (declare (ignore newline)))

(defmethod emit-latex (stream (thing string) &key newline)
  (format stream "~A~:[~;~%~]" thing newline))

(defmethod emit-latex (stream (thing character) &key newline)
  (emit-latex stream (string thing) :newline newline))

(defmethod emit-latex (stream (thing cons) &key (newline nil newline-supplied-p))
  (cond ((atom (car thing))
         (apply #'emit-latex-gf stream (car thing) (cdr thing)
                (when newline-supplied-p `(:newline ,newline))))
        ((listp (car thing))
         (apply #'emit-latex-gf stream (caar thing) (cdr thing)
                (when newline-supplied-p `(:newline ,newline))))))
               
(defmethod emit-latex (stream (thing (eql :nbsp)) &key (newline nil))
  (emit-latex stream "~" :newline newline))

(defmethod emit-latex (stream (thing (eql :qquad)) &key (newline nil))
  (emit-latex stream "\\qquad" :newline newline))

(defmethod emit-latex (stream (thing (eql :quad)) &key (newline nil))
  (emit-latex stream "\\quad" :newline newline))

(defun emit-latex-freshline (stream)
  (format stream "~&"))

(defun emit-latex-newline (stream)
  (format stream "~%"))

;;; default is a no-op
(defmethod emit-latex-gf (stream type children &key newline)
  (declare (ignore newline)))

(defun emit-children-to-string (children)
  (cond ((null children) nil)
        ((listp children)
         (apply #'concatenate 'string
                (loop for c in children collect (emit-latex nil c))))
        (t children)))

(defmethod emit-latex-gf (stream (type (eql :p)) children &key (newline t))
  (emit-latex-freshline stream)
  (dolist (c children)
    (emit-latex stream c))
  (when (or newline t)
    (emit-latex-newline stream)
    (emit-latex-newline stream)))

(defun emit-latex-command (stream command children
                           &key
                           (newline t)
                           (initial-freshline t)
                           (options))
  (format stream "~:[~;~&~]\\~A~@[[~A]~]~@[{~A}~]~:[~;~%~]"
          initial-freshline
          command
          options
          (cond ((null children) nil)
                ((listp children)
                 (apply #'concatenate 'string
                        (loop for c in children collect (emit-latex nil c))))
                (t children))
          newline))
  
(defun emit-latex-command-2 (stream command &key options arg1 arg2 (newline t))
  (format stream "~&\\~A~@[[~A]~]~@[{~A}~]~@[{~A}~]~:[~;~%~]" command options arg1 arg2 newline))

(defun emit-latex-command-3 (stream command section &key options arg1 arg2 (newline t))
  (format stream "~&\\~A{~A}~@[[~A]~]~@[{~A}~]~@[{~A}~]~:[~;~%~]" command section options arg1 arg2 newline))

(defun emit-latex-command-4 (stream command children
                           &key
                           (newline t)
                           (initial-freshline t)
                           (options))
  (format stream "~:[~;~&~]\\~A~@[[~A]~]{"
          initial-freshline
          command
          options)
  (loop for c in children do (emit-latex stream c))
  (format stream "}~:[~;~%~]" newline))

(defun emit-latex-parameter (stream command children &key (newline t))
  (format stream "~&\\~A~@[ ~A~]~:[~;~%~]"
          command
          (cond ((null children) nil)
                ((listp children)
                 (apply #'concatenate 'string
                        (loop for c in children collect (emit-latex nil c))))
                (t children))
          newline))

(defmethod emit-latex-gf (stream (type (eql :div)) children &key newline)
  (loop for c in children collect
       (emit-latex stream c))
  (when newline (emit-latex-newline stream)))

(defmethod emit-latex-gf (stream (type (eql :span)) children &key newline)
  #+nil (format stream "~{~A~}~:[~;~%~]"
                (loop for c in children collect (emit-latex nil c))
                newline)
  (loop for c in children collect (emit-latex stream c)))

(defun emit-latex-block (command stream children &key newline)
  (format stream "{\\~A ~{~A~}}~:[~;~%~]" command
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmacro with-latex-block (command stream &rest rest)
  `(progn
     (format ,stream "{\\~A " ,command)
     ,@rest
     (format ,stream "}~:[~;~%~]" t)))

(defmethod emit-latex-gf (stream (type (eql :i)) children &key newline)
  (emit-latex-block "it" stream children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :b)) children &key newline)
  (emit-latex-block "bf" stream children :newline newline))

(defparameter *document-single-space-count* 0)

(defun single-space (stream)
  (if *document-thesis*
      (progn
        (incf *document-single-space-count*)
        (emit-latex stream "\\ssp" :newline t))
      (emit-latex stream (format nil "\\baselineskip~A" "12pt") :newline t)))

(defun default-space (stream)
  (if *document-thesis*
      (progn
        (unless (plusp (decf *document-single-space-count*))
          (emit-latex stream (format nil "\\dsp") :newline t)))
      (emit-latex stream (format nil "\\baselineskip~A" *baseline-skip*) :newline t)))

(defmethod emit-latex-gf (stream (type (eql :pre)) children &key (newline nil))
  (declare (ignore newline))
  (emit-latex-newline stream)
  (single-space stream)
  (emit-latex-command stream "begin" '("verbatim") :newline nil)
  (format stream "~{~A~}"
          (loop for c in children collect (emit-latex nil c)))
  (emit-latex-command stream "end" '("verbatim"))
  (default-space stream))


(defmethod emit-latex-gf (stream (type (eql :code)) children &key (newline nil))
  (format stream "~{~A~}~:[~;~%~]"
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmethod emit-latex-gf (stream (type (eql :pseudocode)) children &key (newline))
  (destructuring-bind (&key name (parameters " "))
      (car children)
    (single-space stream)
    (emit-latex-command-3 stream "begin" "pseudocode" :options "framebox" :arg1 name :arg2 parameters :newline nil)
    (format stream "~{~A~}~:[~;~%~]"
            (loop for c in (cdr children)
               collect (emit-latex nil c))
            newline)
    (emit-latex-command stream "end" "pseudocode" :newline newline)
    (default-space stream)))

(defmethod emit-latex-gf (stream (type (eql :soutput)) children &key (newline))
  (declare (ignore newline))
  (single-space stream)
  (emit-latex stream "{\\scriptsize")
  (emit-latex-command stream "begin" "Soutput")
  (dolist (c children)
    (emit-latex stream c))
  (emit-latex-command stream "end" "Soutput")
  (emit-latex stream "}" :newline t)
  (default-space stream))

(defmethod emit-latex-gf (stream (type (eql :results)) children &key (newline nil))
  (format stream "~{~A~}~:[~;~%~]"
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmethod emit-latex-gf (stream (type (eql :clearpage)) children &key (newline t))
  (emit-latex-command stream "clearpage" nil :newline t))

(defparameter *article-headings* '((:h1 . "section")
                                   (:h2 . "subsection")
                                   (:h3 . "subsubsection")
                                   (:h4 . "paragraph")))

(defparameter *thesis-headings* '((:h1 . "chapter")
                                  (:h2 . "section")
                                  (:h3 . "subsection")
                                  (:h4 . "subsubsection")))

(defparameter *headings* *article-headings*)

(defun setup-headings ()
  (if *document-thesis*
      (progn
        (setf *document-options* "11pt")
        (setf *headings* *thesis-headings*))
      (progn
        (setf *document-class* "article")
        (setf *document-options* "10pt")
        (setf *headings* *article-headings*))))

(defmethod emit-latex-gf (stream (type (eql :appendices)) children &key (newline t))
  (declare (ignore newline))
  (single-space stream)
  (emit-latex-command stream "appendix" nil)
  (loop for c in children
     do (emit-latex stream c))
  (default-space stream))

(defmethod emit-latex-gf (stream (type (eql :h1)) children &key (newline t))
  (ch-util::with-keyword-args ((label (clearpage t) no-number) children)
      children
    (declare (ignore no-number))
    (when clearpage
      (emit-latex-command stream "clearpage" nil :newline t))
    (when *document-thesis*
      (emit-latex stream "\\pagestyle{fancyplain}" :newline t)
      (emit-latex stream "\\cfoot{}" :newline t))
    (emit-latex-command stream (cdr (assoc type *headings*))
                        children :newline newline)
    (when label
      (emit-latex-command stream "label" label :newline newline))))


(defmethod emit-latex-header (stream type children &key (newline t))
  (ch-util::with-keyword-args ((label clearpage no-number) children)
      children
    (when clearpage
      (emit-latex-command stream "clearpage" nil :newline t))
    (single-space stream)
    (emit-latex-command stream (cdr (assoc type *headings*))
                        children :newline newline)
    (when label
      (emit-latex-command stream "label" label :newline newline))
    (default-space stream)))

(defmethod emit-latex-gf (stream (type (eql :h2)) children &key (newline t))
  (emit-latex-header stream type children :newline newline)
  #+nil (ch-util::with-keyword-args ((label) children)
            children
          (when label
            (emit-latex-command stream 'label label :newline newline))))

(defmethod emit-latex-gf (stream (type (eql :h3)) children &key (newline t))
  (emit-latex-header stream type children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :h4)) children &key (newline t))
  (emit-latex-header stream type children :newline newline))


(defmethod emit-latex-gf (stream (type (eql :bibcite)) children &key (newline nil))
  (emit-latex-command stream "cite" (format nil "~{~A~^, ~}" children) :newline newline))

(defmethod emit-latex-gf (stream (type (eql :caption)) children &key (newline nil))
  (emit-latex-command stream "caption" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :label)) children &key (newline nil))
  (emit-latex-command stream "label" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :ref)) children &key newline)
  (declare (ignore newline))
  (emit-latex-command stream "ref" children :initial-freshline nil :newline nil))

(defmethod emit-latex-gf (stream (type (eql :nbsp)) children &key newline)
  (declare (ignore newline children))
  (emit-latex stream "~"))

(defmethod emit-latex-gf (stream (type (eql :centering)) children &key (newline nil))
  (emit-latex-command stream "centering" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :image)) children &key (newline t))
  (destructuring-bind (image-pathname &key
                                      (width)
                                      &allow-other-keys)
      children
    (let ((image-file (ch-util:unix-name image-pathname)))
      (apply #'emit-latex-command-2
             stream
             "includegraphics"
             (list*
              :arg1 image-file
              :newline newline
              (when width `(:options ,(format nil "width=~A" width))))))))
  
(defmethod emit-latex-gf (stream (type (eql :figure)) children &key (newline t) (placement "htbp"))
  (declare (ignorable newline))
  (ch-util::with-keyword-args (((placement placement) label) children)
      children
    (emit-latex-command-3 stream "begin" "figure" :options placement :newline nil)
    (dolist (p children)
      (emit-latex stream p :newline nil))
    (when label
      (emit-latex-command stream "label" label :newline t))
    (emit-latex-command stream "end" "figure")))

(defmethod emit-latex-gf (stream (type (eql :subfigure)) children &key (newline t))
  (ch-util::with-keyword-args ((caption) children)
      children
    (when caption
      (setf caption (emit-children-to-string caption)))
    (apply #'concatenate 'string
           (apply #'emit-latex-command
                  stream "subfigure"
                  children
                  (append
                   (when caption `(:options ,caption))
                   `(:newline ,newline)))
           (unless caption
             (list (emit-latex-command-2 stream "addtocounter" :arg1 "subfigure" :arg2 "-1"))))))

(defmethod emit-latex-gf (stream (type (eql :document-element)) children &key (newline t))
  (destructuring-bind (element &rest rest) children
    (emit-latex-command-3 stream "begin" element :newline newline)
    (dolist (p (remove-from-plist rest :clearpage))
      (emit-latex stream p :newline nil))
    (when (string-equal element "abstract")
      (emit-latex-command stream "abstractsignature" nil))
    (emit-latex-command stream "end" element)))



(defparameter *latex-packages*
  '("amssymb" "amsmath" "verbatim" "graphicx" "subfigure" "hyperref" "fancyheadings" "longtable"
    ("geometry" . "letterpaper")))
;;; "scicite" "pslatex" "times" "epsfig" "graphs" "newcent"
   
(defun include-contents-of-file-file (stream file)
  (emit-latex stream (ch-util::contents-of-file file)))

(defparameter *document-format-parameters*
  '(("oddsidemargin" . "0.5in")
    ("textwidth" . "6.0in")
    ("topmargin" . "0in")
    ("headheight" . "0.1in")
    ("headsep" . "0.0in")
    ("textheight" . "9.6in")
    ("footskip" . "0.4in")
    ("parindent" . "0.5in")))

(defparameter *section-numbering-depth* 5)

(defparameter *document-class* "article")
(defparameter *document-options* "10pt")

(defparameter *document-latex-commands*
  '("\\newcommand{\\argmax}{\\operatornamewithlimits{argmax}}"
    "\\newcommand{\\argmin}{\\operatornamewithlimits{argmin}}"))

(defun latex-document-format (stream)
  (dolist (package *latex-packages*)
    (if (consp package)
        (emit-latex-command-2 stream "usepackage" :options (cdr package) :arg1 (car package))
        (emit-latex-command stream "usepackage" package)))
  (loop for (param . val) in *document-format-parameters*
     do (emit-latex-parameter stream param val))
  (dolist (command *document-latex-commands*)
    (emit-latex stream command)))

(defun latex-document (stream sexp &key (options *document-options*) (class *document-class*))
  (emit-latex-command-2 stream "documentclass"
                        :options options :arg1 class :newline t)
  (emit-latex-command-2 stream "setcounter"
                        :arg1 "secnumdepth"
                        :arg2 *section-numbering-depth* :newline t)
  (latex-document-format stream)

  (when *document-title*
    (emit-latex-command stream "title" (list *document-title*)))
  (when *document-author*
    (emit-latex-command stream "author" (list *document-author*)))
  (when *document-date*
    (emit-latex-command stream "date" (list *document-date*)))
  (when *document-thesis*
    (when *document-degree-year*
      (emit-latex-command stream "degreeyear" (list *document-degree-year*)))
    (when *document-degree-semester*
      (emit-latex-command stream "degreesemester" (list *document-degree-semester*)))
    (when *document-degree*
      (emit-latex-command stream "degree" (list *document-degree*)))
    (when *document-chair*
      (emit-latex-command stream "chair" (list *document-chair*)))
    (when *document-other-members*
      (emit-latex-command stream "othermembers" (list *document-other-members*)))
    (when *document-number-of-members*
      (emit-latex-command stream "numberofmembers" (list *document-number-of-members*)))
    (when *document-prev-degrees*
      (emit-latex-command stream "prevdegrees" (list *document-prev-degrees*)))
    (when *document-field*
      (emit-latex-command stream "field" (list *document-field*)))
    (when *document-campus*
      (emit-latex-command stream "campus" (list *document-campus*))))
  
  (if *document-thesis*
      (progn
        (emit-latex stream
                    (format nil
                            "\\def\\dsp{\\def\\baselinestretch{~A}\\large\\normalsize}"
                            *baseline-stretch*)
                    :newline t)
        (emit-latex stream "\\dsp" :newline t)
        
        (emit-latex stream "\\addtolength{\\headheight}{\\baselineskip}" :newline t)
        #+nil
        (progn
          (emit-latex stream "\\lhead[\\fancyplain{}\\sl\\thepage]{\\fancyplain{}\\sl\\rightmark}" :newline t)
          (emit-latex stream "\\rhead[\\fancyplain{}\\sl\\leftmark]{\\fancyplain{}\\sl\\thepage}" :newline t)
          (emit-latex stream "\\lhead[\\fancyplain{}\\bfseries\\thepage]{\\fancyplain{}\\bfseries\\rightmark}" :newline t)
          (emit-latex stream "\\rhead[\\fancyplain{}\\bfseries\\leftmark]{\\fancyplain{}\\bfseries\\thepage}" :newline t))
        (progn (emit-latex stream "\\lhead[\\fancyplain{}{}]{\\fancyplain{}{\\bfseries\\leftmark}}" :newline t)
               (emit-latex stream "\\rhead[\\fancyplain{}{}]{\\fancyplain{\\bfseries\\thepage}{\\bfseries\\thepage}}" :newline t))
        (emit-latex stream "\\hyphenpenalty=1000" :newline t)
        (emit-latex stream "\\clubpenalty=500" :newline t)
        (emit-latex stream "\\widowpenalty=500" :newline t))
      (progn
        (emit-latex stream "\\geometry{verbose,tmargin=1in,bmargin=1in,lmargin=1in,rmargin=1in}" :newline t)))
  
  (emit-latex-command stream "begin" "document")
  (emit-latex-freshline stream)

  (emit-latex stream "\\maketitle" :newline t)

  (emit-latex stream "\\let\\mypdfximage\\pdfximage" :newline t)
  (emit-latex stream "\\def\\pdfximage{\\immediate\\mypdfximage}" :newline t)
  
  (if *document-thesis*
      (progn
        (emit-latex stream "\\approvalpage" :newline t)
        (emit-latex stream "\\copyrightpage" :newline t))
      (emit-latex stream (format nil "\\baselineskip~A" *baseline-skip*) :newline t))
  
  (dolist (p sexp)
    (emit-latex stream p))

  (emit-latex-command stream "end" "document"))

(defun latex-use-package (stream package)
  (emit-latex-command stream "usepackage" package))

(defun latex-use-packages (stream &rest packages)
  (mapcar #'(lambda (x) (latex-use-package stream x)) packages))

;;; Bibliography stuff

(defun latex-cite (stream ref)
  (emit-latex-command stream "cite" ref))

(defun latex-bibliography-style (stream style)
  (emit-latex-command stream "bibliographystyle" style))

(defun latex-bibliography (stream bib)
  (emit-latex-command stream "bibliography" bib))

;;; The main entry point to this stuff

(defmethod render-as ((type (eql :latex)) sexp file)
  (let ((*document-thesis* *document-thesis*)
        (*document-class* *document-class*)
        (*document-degree-year* *document-degree-year*)
        (*document-degree-semester* *document-degree-semester*)
        (*document-degree* *document-degree*)
        (*document-chair* *document-chair*)
        (*document-other-members* *document-other-members*)
        (*document-number-of-members* *document-number-of-members*)
        (*document-prev-degrees* *document-prev-degrees*)
        (*document-field* *document-field*)
        (*document-campus* *document-campus*)
        (*headings* *headings*)
        (*latex-packages* *latex-packages*)
        (*document-format-parameters* *document-format-parameters*))
    (setup-headings))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (latex-document stream sexp)))


;; the intent here is that :bibliography is a tag with no children and
;; that we should output the appropriate latex bibliography here.
(defmethod emit-latex-gf (stream (type (eql :bibliography)) children &key (newline t))
  (declare (ignore newline))
  (destructuring-bind (&rest rest &key (clearpage t) &allow-other-keys)
      children
    (declare (ignore rest))
    (when clearpage
      (emit-latex-command stream "clearpage" nil :newline t)))
  (emit-latex stream (format nil "\\baselineskip~A" "11pt") :newline t)
  (let ((style-function (bibtex-compiler:find-bibtex-style *bibtex-style*))
      (bibtex-runtime:*cite-keys* (reverse *cite-keys*))
      (bibtex-runtime:*bib-macros* *bibtex-macros*)
      (bibtex-runtime:*bib-database* *bibtex-database*)
      (bibtex-runtime:*bib-files* nil)
      (bibtex-runtime:*bbl-output* stream))
  (funcall style-function)))

(defmethod emit-latex-gf (stream (type (eql :table-of-contents)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex stream "\\tableofcontents" :newline t))

(defmethod emit-latex-gf (stream (type (eql :list-of-figures)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex stream "\\listoffigures" :newline t))

(defmethod emit-latex-gf (stream (type (eql :table-of-tables)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex stream "\\listoftables" :newline t))


;;; equations

;; FIXME!!!

(defmethod emit-latex-gf (stream (type (eql :equation)) children &key (newline t))
  (declare (ignorable newline))
  (emit-latex-command-3 stream "begin" "equation" :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" "equation" :newline t))

;;; tables

(defmethod emit-latex-gf (stream (type (eql :table)) children &key newline)
  (destructuring-bind ((&key cols
                             top-line) (&rest children))
      (apply #'ch-util::remove-keywordish-args '(:cols
                                                 :top-line) children)
    (emit-latex-command-3 stream "begin" "tabular" :arg1 cols)
    (when top-line
      (emit-latex stream (format nil "\\hline~%")))
    (loop for c in children collect
         (emit-latex stream c))
    (emit-latex-command-3 stream "end" "tabular")
    (when newline (emit-latex-newline stream))))

(defun emit-table-row (stream children &key (newline t))
  (emit-latex stream (format nil "~{~A~^ & ~}\\\\"
                             (mapcar #'(lambda (x)
                                         (emit-latex nil x))
                                     children)) :newline newline))

(defmethod emit-latex-gf (stream (type (eql :longtable)) children &key newline)
  (destructuring-bind ((&key cols
                             top-line
                             heading
                             caption
                             (first-heading heading)
                             (first-caption caption)
                             (font-size "small")) (&rest children))
      (apply #'ch-util::remove-keywordish-args '(:cols
                                                 :top-line
                                                 :heading
                                                 :caption
                                                 :first-heading
                                                 :first-caption
                                                 :font-size) children)
    
    (single-space stream)
    (with-latex-block font-size
      stream
      (emit-latex-command-3 stream "begin" "longtable" :arg1 cols)

      (when first-caption
        (destructuring-bind (first-caption)
            first-caption
          (emit-latex-command stream "caption" first-caption :newline nil)
          (emit-latex stream "\\\\" :newline t)))

      (when top-line
        (emit-latex stream "\\hline" :newline t))

      (when first-heading
        (emit-table-row stream (car first-heading))
        (loop for c in (cdr first-heading)
           do (emit-latex stream c)))
      (emit-latex stream "\\endfirsthead" :newline t)

      (when caption
        (destructuring-bind (caption)
            caption
          (emit-latex-command stream "caption" caption :newline nil)
          (emit-latex stream "\\\\" :newline t)))

      (when top-line
        (emit-latex stream "\\hline" :newline t))

      (when heading
        (emit-table-row stream (car heading))
        (loop for c in (cdr heading)
           do (emit-latex stream c)))
      (emit-latex stream "\\endhead" :newline t)

      (loop for c in children collect
           (emit-latex stream c))
      (emit-latex-command-3 stream "end" "longtable")
      (when newline (emit-latex-newline stream)))
    (default-space stream)))

(defmethod emit-latex-gf (stream (type (eql :table-row)) children &key (newline t))
  (destructuring-bind ((&key multicolumn (spec "|c|")) (&rest children))
      (apply #'ch-util::remove-keywordish-args '(:multicolumn :spec) children)
    (if multicolumn
        (progn (apply #'emit-latex-command-3 stream "multicolumn" multicolumn
                      :arg1 spec
                      :newline nil
                      :arg2 (mapcar #'(lambda (x)
                                        (emit-latex nil x))
                                    children))
               (emit-latex stream "\\\\" :newline newline))
        (emit-latex stream (format nil "~{~A~^ & ~}\\\\"
                                   (mapcar #'(lambda (x)
                                               (emit-latex nil x))
                                           children)) :newline newline))))

(defmethod emit-latex-gf (stream (type (eql :horizontal-line)) children &key (newline t))
  (emit-latex stream "\\hline" :newline newline))
