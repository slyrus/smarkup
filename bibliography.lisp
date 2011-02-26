;;
;; Copyright (c) 2007, Cyrus Harmon

(in-package :smarkup)

(defparameter *bibtex-macros* (make-hash-table :test #'equalp))
(defparameter *bibtex-database* (make-hash-table :test #'equalp))
(defparameter *bibtex-style* nil)
(defparameter *cite-keys* nil)
(defparameter *cite-order* (make-hash-table :test #'equalp))
(defparameter *current-citation* 0)

(defun cite-text (cite)
  (let ((cite-hash (gethash cite *bibtex-database*)))
    (when cite-hash
      (format nil "~@[~A. ~]~@[~A ~]~@[~A ~]~@[~A ~]~@[~A ~]~@[(~A).~]"
              (bibtex-runtime::bib-entry-ref "author" cite-hash)
              (bibtex-runtime::bib-entry-ref "title" cite-hash)
              (bibtex-runtime::bib-entry-ref "journal" cite-hash)
              (bibtex-runtime::bib-entry-ref "volume" cite-hash)
              (bibtex-runtime::bib-entry-ref "number" cite-hash)
              (bibtex-runtime::bib-entry-ref "year" cite-hash)))))

(defun get-cite-keys ()
  (mapcar #'car
          (sort
           (loop for k being the hash-keys of *cite-order* using (hash-value v)
              collect (cons k v))
           #'<
           :key #'cdr)))

