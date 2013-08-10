(defun find-even (L)
	"Given a list L of members, return the leftmost even number."
	(if (null L)
		NIL
		(if (evenp (first L))
			(first L)
		(find-even (rest L)))))
