(defun fast-recursive-list-length (L)
	"A tail-recursive version of recursive-list-length."
	(fast-recursive-list-length-aux L 0))

(defun fast-recursive-list-length-aux (L A)
	(if (null L)
		A
		(fast-recursive-list-length-aux (rest L) (1+ A))))
