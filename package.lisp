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

	   :tagged?
	   :tag?
	   
	   :tagged-tag
	   :tagged-nodes
	   :tag-name
	   :attr-val-plist

	   :text-only

	   :head
	   :tail
	   :empty-pipe-p
	   :pipe-elt
	   
	   )

  )


