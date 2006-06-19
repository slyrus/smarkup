;;
;; Copyright (c) 2006, Cyrus Harmon
;;
;; latex.cl - latex output from smarkup
;;

(in-package :smarkup)

(defparameter *baseline-skip* "21pt")
(defparameter *baseline-stretch* "1.9")
(defparameter *par-skip* "21pt")
(defparameter *latex-graphics-params* nil)
(defparameter default-image-width "1.4in")

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
               
(defun emit-latex-freshline (stream)
  (format stream "~&"))

(defun emit-latex-newline (stream)
  (format stream "~%"))

;;; default is a no-op
(defmethod emit-latex-gf (stream type children &key newline)
  (declare (ignore newline)))

(defmethod emit-latex-gf (stream (type (eql :p)) children &key (newline t))
  (emit-latex-freshline stream)
  (dolist (c children)
    (emit-latex stream c))
  (when (or newline t)
    (emit-latex-newline stream)
    (emit-latex-newline stream)))

(defun emit-latex-command (stream command children &key (newline t))
  (format stream "~&\\~A~@[{~A}~]~:[~;~%~]"
          command
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
  (format stream "~{~A~}~:[~;~%~]"
          (loop for c in children collect (emit-latex nil c))
          newline))

(defun emit-latex-block (command stream children &key newline)
  (format stream "{\\~A ~{~A~}}~:[~;~%~]" command
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmethod emit-latex-gf (stream (type (eql :i)) children &key newline)
  (emit-latex-block "it" stream children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :b)) children &key newline)
  (emit-latex-block "bf" stream children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :pre)) children &key (newline nil))
  (declare (ignore newline))
  (emit-latex-newline stream)
  (if *document-thesis*
      (emit-latex stream "\\ssp" :newline t)
      (emit-latex stream (format nil "\\baselineskip~A" "12pt") :newline t))
  (emit-latex-command stream "begin" '("verbatim") :newline nil)
  (format stream "~{~A~}"
          (loop for c in children collect (emit-latex nil c)))
  (emit-latex-command stream "end" '("verbatim"))
  (if *document-thesis*
      (emit-latex stream (format nil "\\dsp") :newline t)
      (emit-latex stream (format nil "\\baselineskip~A" *baseline-skip*) :newline t)))


(defmethod emit-latex-gf (stream (type (eql :code)) children &key (newline nil))
  (format stream "~{~A~}~:[~;~%~]"
          (loop for c in children collect (emit-latex nil c))
          newline))

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
        (setf *headings* *thesis-headings*))
      (progn
        (setf *document-class* "article")
        (setf *headings* *article-headings*))))

(defmethod emit-latex-gf (stream (type (eql :h1)) children &key (newline t))
  (destructuring-bind (heading &rest rest &key (clearpage t) &allow-other-keys) children
    (when clearpage
      (emit-latex-command stream "clearpage" nil :newline t))
    (when *document-thesis*
      (emit-latex stream "\\pagestyle{fancyplain}" :newline t)
      (emit-latex stream "\\cfoot{}" :newline t))
    (if (and *document-thesis* (string-equal heading "Appendices"))
        (emit-latex-command stream "appendix" nil)
        (emit-latex-command stream (cdr (assoc type *headings*))
                            (cons heading (remove-from-plist rest :clearpage)) :newline newline))))


(defmethod emit-latex-header (stream type children &key (newline t))
    (destructuring-bind (heading &rest rest &key clearpage &allow-other-keys) children
      (when clearpage
        (emit-latex-command stream "clearpage" nil :newline t))
      (emit-latex-command stream (cdr (assoc type *headings*))
                          (cons heading (remove-from-plist rest :clearpage)) :newline newline)))

(defmethod emit-latex-gf (stream (type (eql :h2)) children &key (newline t))
  (emit-latex-header stream type children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :h3)) children &key (newline t))
  (emit-latex-header stream type children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :h4)) children &key (newline t))
  (emit-latex-header stream type children :newline newline))


(defmethod emit-latex-gf (stream (type (eql :bibcite)) children &key (newline nil))
  (emit-latex-command stream "cite" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :caption)) children &key (newline nil))
  (emit-latex-command stream "caption" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :centering)) children &key (newline nil))
  (emit-latex-command stream "centering" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :image)) children &key (newline t))
  (destructuring-bind (image-pathname &key
                                      (width default-image-width)
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
  
(defmethod emit-latex-gf (stream (type (eql :figure)) children &key (newline t))
  (declare (ignorable newline))
  (emit-latex-command-3 stream "begin" "figure" :options "htbp" :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" "figure"))

(defmethod emit-latex-gf (stream (type (eql :subfigure)) children &key (newline t))
  (emit-latex-command stream "subfigure" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :document-element)) children &key (newline t))
  (destructuring-bind (element &rest rest) children
    (emit-latex-command-3 stream "begin" element :newline newline)
    (dolist (p (remove-from-plist rest :clearpage))
      (emit-latex stream p :newline nil))
    (when (string-equal element "abstract")
      (emit-latex-command stream "abstractsignature" nil))
    (emit-latex-command stream "end" element)))

(defparameter *latex-packages*
  '("amssymb" "amsmath" "verbatim" "graphicx" "subfigure" "hyperref" "fancyheadings" "geometry"))
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
(defparameter *document-options* "12pt")

(defun latex-document-format (stream)
  (dolist (package *latex-packages*)
    (emit-latex-command stream "usepackage" package))
  (loop for (param . val) in *document-format-parameters*
     do (emit-latex-parameter stream param val)))

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
  
  (when *document-thesis*
    (emit-latex stream
                (format nil
                        "\\def\\dsp{\\def\\baselinestretch{~A}\\large\\normalsize}"
                        *baseline-stretch*)
                :newline t)
    (emit-latex stream "\\dsp" :newline t)

    (emit-latex stream "\\geometry{verbose,tmargin=1.2in,bmargin=1.3in,lmargin=1.5in,rmargin=1in}" :newline t)
    (emit-latex stream "\\addtolength{\\headheight}{\\baselineskip}" :newline t)
    (emit-latex stream "\\lhead[\\fancyplain{}\\sl\\thepage]{\\fancyplain{}\\sl\\rightmark}" :newline t)
    (emit-latex stream "\\rhead[\\fancyplain{}\\sl\\leftmark]{\\fancyplain{}\\sl\\thepage}" :newline t)
    (emit-latex stream "\\lhead[\\fancyplain{}\\bfseries\\thepage]{\\fancyplain{}\\bfseries\\rightmark}" :newline t)
    (emit-latex stream "\\rhead[\\fancyplain{}\\bfseries\\leftmark]{\\fancyplain{}\\bfseries\\thepage}" :newline t)
    (emit-latex stream "\\hyphenpenalty=1000" :newline t))
  
  (emit-latex-command stream "begin" "document")
  (emit-latex-freshline stream)

  (emit-latex stream "\\maketitle" :newline t)

  (emit-latex stream "\\let\\mypdfximage\\pdfximage" :newline t)
  (emit-latex stream "\\def\\pdfximage{\\immediate\\mypdfximage}" :newline t)
  
  (when *document-thesis*
    (emit-latex stream "\\approvalpage" :newline t)
    (emit-latex stream "\\copyrightpage" :newline t))
  
  #+nil (emit-latex stream (format nil "\\baselineskip~A" *baseline-skip*) :newline t)
  
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
  (emit-latex stream (format nil "\\baselineskip~A" "12pt") :newline t)
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

