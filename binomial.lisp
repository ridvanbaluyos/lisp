(defun binomial (N R)
	(if (< N R)
		'(error "N should be greater than R!")
		(if (or (= R 0) (= R N))
			1
			(+ (binomial (1- N) (1- R)) (binomial (1- N) R))
		)	
	)
)
