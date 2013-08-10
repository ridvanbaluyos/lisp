(defun sum (L)
	"A linearly recursive implementation that computes the sum of all numbers in a list L."
	(if (null L) 
		0
		(+ (first L) (sum (rest L)))
	)
)

(defun sum2 (L A)
	(if (null L)
	    A
		(sum2 (rest L) (+ A (first L)))
	)
)

(defun sum-tail-recursive (L)
	(sum2 L 0)
)
