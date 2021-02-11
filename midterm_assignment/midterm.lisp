;;;; Prolog Interpreter

;;; Program that reads a list that
;;; contains a parsed prolog program
;;; and proves if the queries are true

;; Author: Rahmet Ali Olmez 
;; January 2021

;this is only used for testing, does not affect program 
(setf resultg '())

(defun file-to-list (fileName)
	(let (newList)
		(with-open-file (inStream fileName)
			(loop for element = (read inStream nil) until (null element) 
				do
            		(setf newList (append newList  element))))
	(return-from file-to-list newList)))

(defun is-query(list0)
	(if (null (car list0))
		(return-from is-query t)))

(defun equal-param-count(query other)
	(if (= (length (cadr query)) (length (cadar other)))
		(return-from equal-param-count t)
		(return-from equal-param-count nil)))

(defun is-var(a0)
	(if (not (stringp a0))
		(return-from is-var nil))
	(if (upper-case-p (char a0 0))
		(return-from is-var t))
		(return-from is-var nil))

(defun unify(query other)
	(let (result c temp)
	;traverse parameters
	;;(format t "qu: ~a --- ot: ~a~%" query other)
	(setf c 0)
	(setf temp (cadr other))
	(loop for i in (car (cdr query))
		do
			(progn
				;;(format t "q: ~a --- o: ~a~%" i (nth c temp))
				(if (is-var i)															;if query param is a var
					(progn
						(setf resultg (append resultg (list (list i (nth c temp)))))
						(setf result (append result (list (cons i (nth c temp)))))
						;;(format t "current result: ~a~%" result)
						)
					(if (is-var (nth c temp))											;if other param is a var
						(progn
							(setf resultg (append resultg (list (list (nth c temp) i))))
							(setf result (append result (list (cons (nth c temp) i))))
							;;(format t "current result: ~a~%" result)
							)
						(if (not (equal i (nth c temp)))								;if neither is a var, check equality
							(return-from unify nil))))	
																						
				(setf c (+ c 1))))
	(setf result (append result (list (list "YES."))))
	;;(format t "res: ~a~%" result)
	(return-from unify result)))

(defun set-vars(query results)
	;;(format t "results to bind: ~a to query: ~a~%" results query)
	(let (new-query c pair)
	(setf new-query query)
	(setf c 0)
	;;(format t "setq: ~a setres: ~a~%" query results)
	;for every parameter of query, if there is the same var of query in results, initialize var of query
	(loop for i in (cadr query)
		do
			(progn
				;;(format t "i: ~s resultss: ~a~%" i results)
				(if (or (assoc i results :test #'equal) (rassoc i results :test #'equal))
					(progn
						;;(format t "assoc: ~a~%" (assoc i results :test #'equal))
						;;(format t "rassoc: ~a~%" (rassoc i results :test #'equal))
						(if (null (setf pair (assoc i results :test #'equal)))
							(setf pair (rassoc i results :test #'equal)))		;returns the list of var-val pair
						;;(format t "pair: ~a~%" pair)
						(if (equal (car pair) i)
							(progn (setf (nth c (cadr new-query)) (cdr pair)))
							(setf (nth c (cadr new-query)) (car pair)))))
				(setf c (+ c 1))))
	;;(format t "new-query: ~a~%" new-query)
	(return-from set-vars new-query)))

(defun answer-query(query list0)
	;;(format t "---------------evaluating: ~a~%" query)
	(let (results new-query body-list head-results)
	(loop for i in list0
		do
			(progn
				;;(format t "? ~a~%" i)
				(if (not (is-query i))
					;matching names
					(if (string= (car query) (car (car i)))
						(progn
							;;(format t "name match:	~a~%" i)
							;check if parameter numbers are equal
							(if (equal-param-count query i)
								(progn
									;;(format t "param num same~%")
									;check if body empty (if clause is a fact)
									(if (null (car (cdr i)))
										(progn
											(if (setf results (unify query (car i)))
												(progn
													;;(format t "returning from fact~%")
													(return-from answer-query results)
													)))
										;try to unify head
										(if (setf head-results (unify query (car i)))
											;query the body
											(progn
												(setf body-list (cdr i))
												(setf body-list (car body-list))

												(loop for j in body-list
													do
														(progn 
															;after unifying head, set variables and the unify others (prepare the queries)
															(setf new-query (set-vars j (append head-results results)))
															;;(format t "sending query: ~a~%" new-query)
															;;(format t "in loop j: ~a bodylist: ~a~%" j body-list)
															;(format t "send query: ~a send list0: ~a~%" j list0)
															(setf results (answer-query new-query list0))))
											;(format t "returning result... ~a cdri: ~a~%" results (cdr i))
											(return-from answer-query results))))
									)))))))
	;(format t "final result: ~a~%" results)
	(return-from answer-query results)))

(defun check-list(list0)
	(with-open-file (out-stream "output.txt" :direction :output)
		(loop for i in list0
			do
				(progn
					;(print i)(terpri)
					(if (is-query i)
						(progn
							;(format t "query:		~a~%" (cadr i))
							(format out-stream "RESULT: ~a~%" (answer-query (cadr i) list0))))))))


;;------------Test Cases------------
;;should return yes - test result as expected
;(setf test-list '(
;	(("legs" ("X" 2)) ( ("mammal" ("X")) ("arms" ("X" 2))))
;	(("legs" ("X" 4)) ( ("mammal" ("X")) ("arms" ("X" 0))))
;	(("mammal" ("horse")) ())
;	(("arms" ("horse" 2)) ())
;	(() ("legs" ("horse" 2)))))
;

;;should return horse - test result as expected
;(setf test-list '(
;	(("legs" ("X" 2)) ( ("mammal" ("X")) ("arms" ("X" 2))))
;	(("legs" ("X" 4)) ( ("mammal" ("X")) ("arms" ("X" 0))))
;	(("mammal" ("horse")) ())
;	(("arms" ("horse" 0)) ())
;	(() ("legs" ("Z" 4)))))

;;should return horse - test result as expected
;(
;(("legs" ("X" 2)) ( ("mammal" ("X")) ("arms" ("X" 2))))
;(("legs" ("X" 4)) ( ("mammal" ("X")) ("arms" ("X" 0))))
;(("mammal" ("horse")) ())
;(("arms" ("horse" 0)) ())
;(("power" ("horse")) ())
;(() ("legs" ("Z" 4)))
;(() ("power" ("M")))
;)

;;should return charlie - test result as expected
;(
;(("professor" ("X" "Y")) ( ("teaches" ("X" "C")) ("studies" ("Y" "C"))))
;(("studies" ("charlie" "csc135")) ())
;(("studies" ("olivia" "csc135")) ())
;(("studies" ("jack" "csc131")) ())
;(("studies" ("arthur" "csc134")) ())
;(("teaches" ("kirke" "csc135")) ())
;(("teaches" ("collins" "csc171")) ())
;(("teaches" ("collins" "csc171")) ())
;(("teaches" ("juniper" "csc134")) ())
;(() ("professor" ("kirke" "Students")))
;)

(check-list (file-to-list "input.txt"))