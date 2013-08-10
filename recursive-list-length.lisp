(defun recursive-list-length (L)
	"A recursive implementation  of list-length."
	(if (null L)
		0
		(1+ (recursive-list-length (rest L)))))
