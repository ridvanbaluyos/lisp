#|
#	This script will extract all phones that are of type Java.
#	The source will be a csv file and the output will also be in csv.
#	
#	Usage:
#		You can run the script via the command line using:
#			./javaphones "<filename>.csv" "<filename>"
#
# 		For manual checking, you can run this via the interpreter:
#			(write-to-csv (sort (get-phones-from-file "<filename>.csv") #'> :key #'second) "<filename>")
#			
#	
#	Author: Ridvan Baluyos <ridvan@baluyos.net>
#			(also http://ridvan.baluyos.net/blog)
#
#	Copyright 2009 3rdBrand Pte. Ltd.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxillary and helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-line-values-aux (line A)
	(if (null (search "," line))
		A
		(let
			(
				(next-token (search "," line))
				(rest-line (subseq line (1+ (search "," line))))
			)
			(if (= next-token 0)
					(get-line-values-aux rest-line (append A (list NIL)))
					(get-line-values-aux rest-line (append A (list (subseq line 0 next-token))))
			)
		)
	)
)

(defun replace-key-aux (L key value A)
    (if (null L)
        A
        (let ((front (first L)))
            (if (equal key (first front))
                (replace-key-aux (rest L) key value (append A (list (list key value))))
                (replace-key-aux (rest L) key value (append A (list front)))
            )
        )
    )
)

(defun find-key-aux (L key found)
    (if (and (null found) (not (null L)))
        (if (equal key (first (first L)))
            (find-key-aux (rest L) key (first L))
            (find-key-aux (rest L) key nil)
        )
        found
    )
)

(defun get-phones-from-stream (stream L)
	(let
		((line (read-line stream nil)))
		(if (null line)
			L
			(let 
				((line-values (get-line-values line)))
				(if (and (not (null (first line-values))) (not (null (fifth line-values))) (search "java" (fourth line-values) :test #'char-equal))
					(get-phones-from-stream stream (increment-key L (third line-values) (parse-integer (fifth line-values))))
					(get-phones-from-stream stream L)
				)
			)
		)
	)
)


;;;;;;;;;;;;;;;;;;;;
;; Parent functions
;;;;;;;;;;;;;;;;;;;;

;;
;; A function that writes the list into a csv file. The output
;; file is created on the /tmp folder.
;; 
;; @param L - list of phones.
;;
(defun write-to-csv (L filename)
	(with-open-file (os 
						(make-pathname 
								:name filename :type "csv"
    							:directory '(:absolute "tmp")
						)
						:direction :output
						:if-exists :supersede
						:if-does-not-exist :create
					)
    	(let 
			((*print-pretty* nil))
      		(format os "~{~{~S~^,~}~%~}" L)
		)
	) 
)

;;
;; A function that gets the phone download count from
;; a file. Currentlly filtered by Java phones only.
;;
;; @param file-path - atom, string.
;;
;; returns a list of list.
;;
(defun get-phones-from-file (file-path)
	(with-open-file (stream file-path)
		(get-phones-from-stream stream NIL)
	)
)

;;
;; A function that extracts the values of a given line
;; of string, comma-delimited.
;;
;; @param line - atom, string.
;;
;; returns a list.
;;
(defun get-line-values (line)
	(get-line-values-aux line NIL)
)

;;
;; A function that replaces the value of a specific key in the list.
;; 
;; @param L - a list
;; @param key - a list
;; @param value - atom, string
;;
;; returns a list
;;
(defun replace-key (L key value)
    (replace-key-aux L key value '())
)

;;
;; A function that increments the second element/atom in the given list.
;;
;; @param L - a list
;; @param key - a list
;; @param count - atom, integer/number
;;
;; returns a list
;;
(defun increment-key (L key count)
    (let ((found (find-key L key)))
        (if (null found)
            (append L (list (list key count)))
            (replace-key L key (+ (second found) count))
        )   
    )   
)

;;
;; A function that determines if the key is existing in a give list.
;; 
;; @param L - a list
;; @param key - atom, string
;;
(defun find-key (L key)
    (find-key-aux L key nil)
)

;; 
;; Main Function. 
;;
;; Accepts a command-line argument for filename. Otherwise 
;;
(defun main () 
	(if (< (length *posix-argv*) 3)
		(print "ERROR: Invalid argument length. I'm expecting 2 inputs.")
		(write-to-csv (sort (get-phones-from-file (second *posix-argv*)) #'> :key #'second) (third *posix-argv*))
	)
	(quit :unix-status 0)
)
