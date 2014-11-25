(in-package :tag-match)

;;; utilities
;;; not even close to perfection, but usable 


(defun handle-weird-html-symbols (str)
  ;; Replace frequently used html symbols like
  ;; '&amp;', '&quot;', '&lt;', '&gt;', '&nbsp;'
  ;; with '&', '"', '<', '>', ' ', respectably
  (ppcre:regex-replace-all
   "(&amp;|&quot;|&nbsp;|&lt;|&gt;)"
   str
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
  (when tagged
    (handle-weird-html-symbols 
     (format nil "~{~A~^ ~}"
	     (mapcar #'(lambda (node)
			 (cond ((stringp node) node)
			       ((tag? node) "")
			       ((member (tag-name (tagged-tag node)) remove) (format nil ""))
			       (t (text-only node))))
		     (tagged-nodes tagged))))))



