(defun fast-power (B E)
	"A tail-recursive version of power."
	(fast-power-aux B E 1))

(defun fast-power-aux (B E A)
	(if (= E 0)
		A
		(fast-power-aux B (1- E) (* B A))))
