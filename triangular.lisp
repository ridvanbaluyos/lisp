(defun triangular (N)
	"Compute for the Nth Triangular Number."
	(if (= N 1)
		N
		(+ N (triangular (- N 1)))))
		; or (+ N (triangular (1- N)))))
