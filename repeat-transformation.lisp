(defun repeat-transformation (F N X)
	"Repeat applying function F on object X for N times."
	(if (zerop N)
		X
		(repeat-transformation F (1- N) (funcall F X))))
