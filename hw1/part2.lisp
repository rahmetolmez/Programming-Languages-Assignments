;;;; Primecrawler

;;; Program that reads boundaries from a file
;;; and prints all prime and semi-prime numbers between them

;; Author: Rahmet Ali Olmez 
;; October 2020

(defun is-prime-recursive (a0 modCheck) 
	(if (<= a0 1)
		(return-from is-prime-recursive 0))	;checking below 2
	(if (equalp a0 2)
		(return-from is-prime-recursive 1))	;checking 2
	(if (equalp 0 (mod a0 modCheck))		;not prime, returns 0
		(return-from is-prime-recursive 0))
	(if (equalp modCheck (- a0 1)) 			;prime, returns 1
		(return-from is-prime-recursive 1))
	(incf modCheck)
	(is-prime-recursive a0 modCheck))		;recursive call

(defun is-prime (a0)
	(is-prime-recursive a0 2))

(defun is-prime-print (a0)
	(write (is-prime a0)))

(defun is-semi-prime-recursive (a0 modCheck divisibleCount)
	(if (and (equalp	1 divisibleCount) (equalp 0 (is-prime a0)))
		(return-from is-semi-prime-recursive 0))
	(if (and (equalp 1 (is-prime a0)) (equalp 1 divisibleCount))
			(return-from is-semi-prime-recursive 1))	
	(if (equalp 1 (is-prime a0))
		(return-from is-semi-prime-recursive 0))
	(if (< a0 2)
		(return-from is-semi-prime-recursive 0))
	(if (and (equalp 0 (mod a0 modCheck)) (equalp divisibleCount 0))
		(progn (incf divisibleCount) (setq a0 (/ a0 modCheck)) (incf modCheck)
			(is-semi-prime-recursive a0 modCheck divisibleCount)))
	(incf modCheck)
	(is-semi-prime-recursive a0 modCheck divisibleCount))

(defun is-semi-prime (a0)
	(is-semi-prime-recursive a0 2 0))

(defun is-semi-prime-print (a0)
	(write (is-semi-prime a0)))

(defun primecrawler (a0 a1 outStream)
	(loop for x from a0 to a1
		do (if (equalp 1 (is-prime x))
			(format outStream "~d is Prime~%" x))
		(if (equalp 1 (is-semi-prime x))
			(format outStream "~d is Semi-prime~%" x))))

(defun primecrawler-from-file (fileName)
	(with-open-file (inStream fileName)
		(with-open-file (outStream "primedistribution.txt" :direction :output)
			(primecrawler (read inStream) (read inStream) outStream))))

(primecrawler-from-file "boundaries.txt")