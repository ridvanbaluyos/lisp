(defun factorial (N)
	"Compute the factorial of N."
	(if (= N 1)
		1
		(* N (factorial (- N 1)))))
