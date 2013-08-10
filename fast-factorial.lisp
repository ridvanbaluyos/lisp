(defun fast-factorial (N)
	"A tail-recursive version of factorial."
	(fast-factorial-aux N 1))

(defun fast-factorial-aux (N A)
	"Multiply A by the factorial of N."
	(if (= N 1)
		A
		(fast-factorial-aux (- N 1) (* N A))))
