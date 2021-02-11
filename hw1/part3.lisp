;;;; Collatz Printer

;;; Program to read integers from a file and print collatz
;;; sequence of the integers to a file

;; Author: Rahmet Ali Olmez
;; October 2020

(defun collatz-recursive (a0 outStream)
	(format outStream  "~d " a0)
	(if (<= a0 0) (progn (format outStream  "integers below 1 are not evaluated") 
		(return-from collatz-recursive 0)))
	(if (equalp a0 1) (return-from collatz-recursive 0))
	(if (evenp a0) (setq a0 (/ a0 2)) 
		(setq a0 (+ 1 (* a0 3))))
	(collatz-recursive a0 outStream))

;Reads up to "numberCount" numbers from file
(defun collatz-from-file (fileName numberCount)
	(with-open-file (inStream fileName)
		(with-open-file (outStream "collatz_outputs.txt" :direction :output)
			(loop
				(let (num)
					(if (equalp numberCount 0) (return))
					(setq num (read inStream nil))
					(if (eq num nil) (return))
					(format outStream "~d: " num)
					(collatz-recursive num outStream)
					(format outStream "~%")
					(decf numberCount))))))

(collatz-from-file "integer_inputs.txt" 5)