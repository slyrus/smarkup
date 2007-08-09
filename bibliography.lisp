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
    (format nil "~@[~A. ~]~@[~A ~]~@[~A ~]~@[~A ~]~@[~A ~]~@[(~A).~]"
            (gethash "author" cite-hash)
            (gethash "title" cite-hash)
            (gethash "journal" cite-hash)
            (gethash "volume" cite-hash)
            (gethash "number" cite-hash)
            (gethash "year" cite-hash))))

