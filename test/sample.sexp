((:p
  (:smarkup-metadata 
   (:copyright
    "Copyright 2006, Cyrus Harmon. See LICENSE file.")
   (:title "A Sample Document")
   (:author "Cyrus L. Harmon")
   (:bibtex-database
    (#.(asdf:component-pathname 
        (reduce #'asdf:find-component
                '("smarkup-test" "test" "sample-bib")))))
   (:bibtex-style "Science"))
  (:html-metadata  (:htmlcss "simple.css")))
 
 (:h1 "Testing")
 
 (:p "This is a sample file. It has a " (:bibcite "rubin2000brief") " reference. Neat")

 (:bibliography))

