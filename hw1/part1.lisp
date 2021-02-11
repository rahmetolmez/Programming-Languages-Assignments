;;;; Flattener

;;; Program that reads list from a file and flattens it
;;; converts sub lists to only one list

;; Author: Rahmet Ali Olmez 
;; October 2020

(defun flattener (listInput flattenedList)	
		(loop for x in listInput
    		do
    			(if (not (listp x))
    	  			(progn
    	  				(setf flattenedList (append flattenedList (list x))))
    	  			(setf flattenedList (flattener x flattenedList))))
		(return-from flattener flattenedList))

(defun file-to-list (fileName)
	(let (newList)
		(with-open-file (inStream fileName)
			(loop for element = (read inStream nil) until (null element) 
				do
            		(setf newList (append newList (list element)))))
	(return-from file-to-list newList)))

(defun flattener-test ()
	(with-open-file (outStream "flattened_list.txt" :direction :output)
		(let (flattenedL)
			(setf flattenedL (flattener (file-to-list "nested_list.txt") flattenedL))
			(format outStream "~d" flattenedL))))

(flattener-test)