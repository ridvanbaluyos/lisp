(defun power (B E)
	"Compute for the Eth Power of B."
	(if (= E 1)
		B
		(* B (power B (- E 1)))))
		; or (* B (power B (1- E)))))
