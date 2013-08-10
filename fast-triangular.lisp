(defun fast-triangular (N)
	"A tail-recursive version of Nth Triangular Number."
	(fast-triangular-aux N 1))

(defun fast-triangular-aux (N A)
	(if (= N 1)
		A
		(fast-triangular-aux (1- N) (+ N A))))
