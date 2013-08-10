(defun fibonacci (N)
	"Compute for the Nth Fibonacci Number."
	(if (or (zerop N) (= N 1))
		1
		(let
			((F1 (fibonacci (- N 1)))
			 (F2 (fibonacci (- N 2))))
			(+ F1 F2))))
