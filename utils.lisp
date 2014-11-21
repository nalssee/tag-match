(in-package :tag-match)


(defun file->string (path)
  "Read a whole file and store it in a string"
  (with-open-file (stream path)
    (let ((data (make-array (file-length stream)
			    :element-type 'character :fill-pointer t)))
      (setf (fill-pointer data)
	    (read-sequence data stream))
      data)))


(defun plist? (xs)
  "Not just an ordinary plist, tests if it's an alternating keyword string list
   which represents attribute-value pairs
  "
  (and xs
       (proper-list-p xs)
       (loop for (k v . nil) on xs by #'cddr
	  always (and (keywordp k) (stringp v)))))

