;; Lazy list
(defpackage :tag-match.pipe
  (:use :cl)
  (:export :delay
	   :force
	   :make-pipe
	   :empty-pipe
	   :empty-pipe-p
	   :head
	   :tail
	   :pipe-elt
	   :pipe-append
	   :pipe-filter
	   :pipe-map
	   ))

(defpackage :tag-match
  (:use :cl :alexandria :tag-match.pipe)
  (:export :find-tagged
	   :build-tagged
	   
	   :tag
	   :nodes

	   :head
	   :tail
	   :empty-pipe-p
	   :pipe-elt
	   
	   )

  )


