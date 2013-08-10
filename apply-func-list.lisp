
(defun apply-func-list (L X)
	"Applies the functions in L to X in reversed order."
	(if (= (list-length L) 1)
		(funcall (first L) X)
		(funcall (first L) (apply-func-list (rest L) X))
	)
)
