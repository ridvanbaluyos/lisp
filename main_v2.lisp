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
#				(also http://ridvan.baluyos.net/blog)
#
#	Copyright 2009 3rdBrand Pte. Ltd.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxillary and helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-line-values-aux (line)
	(let
		((token (search "|" line)))
		(list (subseq line 2 (1- token)) (subseq line (+ token 2)))
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
				(if (member (first (string-to-list (second line-values))) (get-java-regular-phone-list))
					(get-phones-from-stream stream (increment-key L (second line-values) 1))
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
	(get-line-values-aux line)
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
;; A function that gets the list of java regular phones. 
;;
;; NOTE: This is kinda pathetic. Source file should have just have a column
;; that determines whether it's a java phone or not. pffft!
;;
(defun get-java-regular-phone-list ()
	;'(Nokia_3110c Nokia_3110 Nokia_3500c Nokia_5200 Nokia_5220 XpressMusic Nokia_5310_XpressMusic Nokia_5610_XpressMusic Nokia_6230i Nokia_6230 Nokia_6233 Nokia_6300 Nokia_6301 Nokia_6500s Nokia_6500 Nokia_6620 Sony_Ericsson_K610i_and_K610c Sony_Ericsson_K610i/K610c SonyEricsson_K610c SonyEricsson_K610iv SonyEricsson_K610i SonyEricsson_K610 Sony_Ericsson_K800i_and_K800c SonyEricsson_K800c SonyEricsson_K800iv SonyEricsson_K800i SonyEricsson_K800 Sony_Ericsson_W810i/W810c SonyEricsson_W810a SonyEricsson_W810c Sony_Ericsson_W810i/c SonyEricsson_W810i Sony_Ericsson_W200i/c Sony_Ericsson_W200i/W200c SonyEricsson_W810 SonyEricsson_W200a SonyEricsson_W200i SonyEricsson_W200c SonyEricsson_W200iv Sony_Ericsson_Z710i Sony_Ericsson_Z710i/c SonyEricsson_Z710 SonyEricsson_Z710c SonyEricsson_Z710i SonyEricsson_Z770i Sony_Ericsson_K700i_and_K700c SonyEricsson_K700i SonyEricsson_K700c SonyEricsson_K700 Samsung_SGH-Z400 Nokia_6600Slide Nokia_6212c Nokia_7510a Nokia_7510supernova Nokia_7100supernova Nokia_7310c Nokia_7310supernova Nokia_7610supernova Nokia_7610 Nokia_7610s Nokia_7210supernova Nokia_7210 Nokia_7210c Nokia_5130 Nokia_5130XpressMusic Nokia_6600Fold Nokia_7070)
	'(Nokia_3110c Nokia_3110 Nokia_3500c Nokia_5200 Nokia_5220 Nokia_5310_XpressMusic Nokia_5610_XpressMusic Nokia_5610d-1 Nokia_6230i Nokia_6230 Nokia_6233 Nokia_6300 Nokia_6301 Nokia_6301b Nokia_6500s Nokia_6500c Nokia_6620 Nokia_6600Slide Nokia_6600s-1c Nokia_6212c Nokia_7510supernova Nokia_7510a Nokia_7100supernova Nokia_7100s._7100s-2 Nokia_7310supernova Nokia_7310c Nokia_7610supernova Nokia_7610s Nokia_7210supernova Nokia_7210c Nokia_5130XpressMusic Nokia_5130 Nokia_6600Fold Nokia_7070 Nokia_7070_Prism Samsung_SGH-Z400 Sony_Ericsson_K610c Sony_Ericsson_K610i/K610c Sony_Ericsson_K610i_and_K610c Sony_Ericsson_K610iv Sony_Ericsson_K610i Sony_Ericsson_K610 Sony_Ericsson_K800c Sony_Ericsson_K800i/K800c Sony_Ericsson_K800i_and_K800c Sony_Ericsson_K800iv Sony_Ericsson_K800i Sony_Ericsson_K800 Sony_Ericsson_W810a Sony_Ericsson_W810c Sony_Ericsson_W810i Sony_Ericsson_W810 Sony_Ericsson_W810i/W810c Sony_Ericsson_W200a Sony_Ericsson_W200i Sony_Ericsson_W200c Sony_Ericsson_W200iv Sony_Ericsson_W200i/c Sony_Ericsson_W200i/W200c Sony_Ericsson_W200i/c Sony_Ericsson_Z710 Sony_Ericsson_Z710c Sony_Ericsson_Z710i/c Sony_Ericsson_Z710i Sony_Ericsson_Z770i Sony_Ericsson_K700i Sony_Ericsson_K700c Sony_Ericsson_K700 Sony_Ericsson_K700i_and_K700c)
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

;;
;; A function that converts a string into a list
;;
;; @param str - atom, string.
;; 
;; returns a list.
;;
(defun string-to-list (str)
   (do* ((stringstream (make-string-input-stream str))
         (result nil (cons next result))
         (next (read stringstream nil 'eos)
               (read stringstream nil 'eos)))
        ((equal next 'eos) (reverse result))
    )   
)
