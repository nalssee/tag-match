(in-package :tag-match)

;; tagged stands for a parsed html (or xml, maybe).
;; tagged := (tag node node ...)
;; node := string 
;;      | tag
;;      | (tag node node ...) ,which is again... "tagged", strings and tags are end-nodes
;; tag := keyword
;;     | (keyword keyword string keyword string ...) ,
;;       the first keyword is the tag name
;;       and the rest is attribute-value pairs


;; when you parse html with cl-html-parse:parse-html, as you see

(defparameter *sample-html* "<html><head><title>The Dormouse's story</title></head> <p class= \"title\"><b>The Dormouse's story</b></p> <p class= \"story\">Once upon a time there were three little sisters; and their names were <a href= \"http://example.com/elsie\" class= \"sister\" id= \"link1\">Elsie</a>, <a href= \"http://example.com/lacie\" class= \"sister\" id= \"link2\">Lacie</a> and <a href= \"http://example.com/tillie\" class= \"sister\" id= \"link3\">Tillie</a>; and they lived at the bottom of a well.</p> <p class= \"story\">...</p>")
;; (cl-html-parse:parse-html *sample-html*)
;; =>

;; ((:HTML (:HEAD (:TITLE "The Dormouse's story")) ((:P :CLASS "title") (:B "The Dormouse's story")) " " ((:P :CLASS "story") "Once upon a time there were three little sisters; and their names were " ((:A :HREF "http://example.com/elsie" :CLASS "sister" :ID "link1") "Elsie") ", " ((:A :HREF "http://example.com/lacie" :CLASS "sister" :ID "link2") "Lacie") " and " ((:A :HREF "http://example.com/tillie" :CLASS "sister" :ID "link3") "Tillie") "; and they lived at the bottom of a well.") ((:P :CLASS "story") "...")))

;; guess what, it generates a list of nodes. it's not 'tagged', so you need to give it tag.
;; and it's name is
(defconstant +root-tag+ :root)



(defclass tagged ()
  ((tag :initarg :tag :reader tag)
   (nodes :initarg :nodes
	  :initform nil
	  :reader nodes)))

(defclass tag ()
  ((name :initarg :name :reader name)
   (attr-val-plist :initarg :attr-val-plist
		   :reader attr-val-plist
		   :initform nil)))


(defmacro define-predicate (class-name)
  "Make a predicate with a name that ends with ? and prefixed with class-name
   (make-predicate complex-node) => complex-node?"
  (let ((obj-param (gensym "OBJ-")))
    `(defun ,(intern (concatenate 'string (string class-name) "?")) (,obj-param)
       (eql ',class-name (type-of ,obj-param)))))

(define-predicate tagged)
(define-predicate tag)

;; 
(defun build-tagged (str)
  "Build a tagged object out of html string"
  (make-instance
   'tagged
   :tag (make-instance 'tag :name +root-tag+)
   :nodes
   ;; loop through nodes
   (loop for x in (cl-html-parse:parse-html str)
      collect (make-tagged x))))


(define-condition invalid-node-error (error)
  ((node :initarg :node)))

(defun make-tagged (parsed-node)
  "'parsed-node' can be just a string or a tag, neither of them can't be a tagged.
    In such cases, just make a leaf, a string or an instance of tag class.
    if it's a valid tagged source, make a tagged object deep down recursively.
  "
  (flet ((compound-tag? (tag)
	   (and (proper-list-p tag) (keywordp (first tag)) (plist? (rest tag)))))
    (cond ((stringp parsed-node) parsed-node)
	  ((keywordp parsed-node) (make-instance 'tag :name parsed-node))
	  ((compound-tag? parsed-node)
	   ;; I don't know if a compound tag can be a node in a valid HTML, I'm just being cautious.
	   (make-instance 'tag :name (first parsed-node)
			  :attr-val-plist (rest parsed-node)))
	  ((and (listp parsed-node) (not (null parsed-node)))
	   ;; (tag node node ...), tagged again
	   (make-instance 'tagged
			  :tag (let ((tag (first parsed-node)))
				 (if (keywordp tag)
				     (make-instance 'tag :name tag)
				     (make-instance 'tag :name (first tag) :attr-val-plist (rest tag))))
			  :nodes (loop for x in (rest parsed-node) collect
				      (make-tagged x))))
	  ;; I'm not throwing away any data samples
	  (t (restart-case (error 'invalid-node-error :node parsed-node)
	       (build-now-node (node) (make-tagged node)))))))


(defparameter +base-offset+ 4)


(defmethod print-object ((tagged tagged) stream)
  (labels ((print-tagged (tagged offset)
	     (loop repeat offset do (format stream " "))	     
	     (print-tag (tag tagged))
	     (terpri stream)
	     (loop for node in (nodes tagged) do
		  (print-node node (+ +base-offset+ offset))
		  (terpri stream)))
	   (print-tag (tag) (format stream "<~{~S~^ ~}>" (cons (name tag) (attr-val-plist tag))))
	   (print-node (node offset)
	     (cond ((stringp node)
		    (loop repeat offset do (format stream " "))
		    (format stream "~S" node))
		   ((tag? node)
		    (loop repeat offset do (format stream " "))
		    (print-tag node))
		   ;; now it's again tag
		   (t (print-tagged node offset)))))
    (print-tagged tagged 0)))


;; Example
;; (build-tagged *sample-html*)


;;;======================================================================================


(defconstant +tag-name-keyword+ :tag-name)

;;; Don't attempt to implement going up the tree.
;;; Sounds fancy but useless.
(defun find-tagged (tagged &rest plist)
  (cond ((or (stringp tagged) (tag? tagged)) ; is it a leaf?
	 empty-pipe)
	((tag-matched? (tag tagged) plist)
	 (make-pipe tagged
		    (apply #'pipe-append
			   (loop for node in (nodes tagged) collect
				(apply #'find-tagged node plist)))))
	(t (apply #'pipe-append
		  (loop for node in (nodes tagged) collect
		       (apply #'find-tagged node plist))))))

(defun tag-matched? (tag plist)
  (cond ((getf plist +tag-name-keyword+)
	 (and (eql (name tag) (getf plist +tag-name-keyword+))
	      (attributes-matched? (attr-val-plist tag)
				   (remove-from-plist plist +tag-name-keyword+))))
	((not (attr-val-plist tag)) nil)
	(t (attributes-matched? (attr-val-plist tag) plist))))

(defun attributes-matched? (plist1 plist2)
  "All attributes in plist2 must be conformant to the ones in plist1"
  (loop for (key . rest) on plist2 by #'cddr
     always (equal (first rest) (getf plist1 key))))





  
(defun handle-weird-html-symbols (str)
  ;; Replace frequently used html symbols like
  ;; '&amp;', '&quot;', '&lt;', '&gt;', '&nbsp;'
  ;; with '&', '"', '<', '>', ' ', respectably
  (ppcre:regex-replace-all
   "(&amp;|&quot;|&nbsp;|&lt;|&gt;)"
   html
   (lambda (match &rest registers)
     (declare (ignore registers))
     (cond ((string-equal match "&amp;") "&")
	   ((string-equal match "&quot;") "\"")
	   ((string-equal match "&nbsp;") " ")
	   ((string-equal match "&lt;") "<")
	   ((string-equal match "&gt;") ">")))
   :simple-calls t))

;; Not perfect but usable at the moment.
(defun text-only (tagged &key (remove '(:table :iframe :img)))
  "Extract only text from tagged and returns a string.
   Losing some of the information (like paragraph delimiters) is inevitable,
   or at least, preferable
   Tags in 'remove' are simply ignored"
  (handle-weird-html-symbols 
   (format nil "~{~A~^ ~}"
	   (mapcar #'(lambda (node)
		       (cond ((stringp node) string)
			     ((tag? node) "")
			     (t (text-only node))))
		   (nodes tagged)))))
