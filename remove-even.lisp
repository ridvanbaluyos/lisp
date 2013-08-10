(defun remove-even (L)
	"Remove all members of L that is an even number."
	(if (null L)
		NIL
		(if (zerop (rem (first L) 2))
			(remove-even (rest L))
			(cons (first L) (remove-even (rest L))))))
