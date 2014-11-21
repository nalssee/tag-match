;;;; This program is not really necessary, closure-html does all that, and does nicer.
;;;; The only reason what I'm writing is because of some unidentified (at least for me)
;;;; illegal HTML files like "error.html" included in this package.
;;;; Search for the literal "can't" and see it is used as an attribute.
;;;; I suspect that it can't be parsed properly with any sane HTML parser and must not be.
;;;; (Beautifulsoup works the same way as closure-html)
;;;; And also for an unidentified reason, 'cl-html-parse' package parses
;;;; those files properly

;;;; The file is downloaded using Python selenium package from Windows
;;;; (or Linux, not sure which one) system. About 10% out of 400k articles
;;;; from "http://seekingalpha.com/articles" are malformed like the file.
;;;; When I downloaded the same page using "Drakma" no such errors were found.

;;;; I am very far from HTML expert So I try to make this program as abstract as possible.
;;;; So that I can fix it later when I'm more knowledgable
;;;; although I hope that all the cases are covered by closure-html



(asdf:defsystem :tag-match
  :depends-on (:cl-html-parse :alexandria)
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "pipe")
	       (:file "tag-match"))
  
  )


