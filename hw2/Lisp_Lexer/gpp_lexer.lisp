;;;; Lexer
;;;  Scans input and outputs tokens
;;;  Not the cleansest implementation,
;;;  but at least it works correctly.

;;   Author: Rahmet Ali Olmez 
;;   November 2020

;reads given file and returns it as a string
(defun file-to-string (fileName)
	(let (text)
  		(with-open-file (inStream fileName)
    		(setf text (make-string (file-length inStream)))
    	  	(read-sequence text inStream))
    	  (return-from file-to-string text)))

;(setq str (file-to-string "input.txt"))

(defun is-space (ch0)
	(if (or (eql ch0 #\space)
			(eql ch0 #\newline)
			(eql ch0 #\tab)
			(eql ch0 #\return))
		(return-from is-space t))
	(return-from is-space nil))

(defun is-parens (ch0)
	(if (or (eql ch0 #\( )
			(eql ch0 #\) ))
		(return-from is-parens t))
	(return-from is-parens nil))

(defun is-quotes (ch0)
	(if (eql ch0 #\" )
		(return-from is-quotes t))
	(return-from is-quotes nil))

(defun test (string0 stream)
	(let (lex0 stat kwFlag quotCounter)
		(setf lex0 "")
		(setf stat "start")
		(setf quotCounter 0)

		;get next word until whitespace or \n or \t or operators
		;if parentheses, print and continue
		(dotimes (iter (length string0))
				(progn
					;(print (char string0 iter))
					(cond
						;handling comments
						((and (string= stat "comment_pos") (not (eql (char string0 iter) #\; ))) (setf stat "comment_error")) 
						((and (eql (char string0 iter) #\newline ) (string= stat "comment_start")) (setf stat "comment_end"))
						((string= stat "comment_start") (setf stat "comment_start"))
						((and (eql (char string0 iter) #\; ) (string= stat "comment_pos")) (setf stat "comment_start"))
						((eql (char string0 iter) #\; ) (setf stat "comment_pos"))

						;handling identifiers
						((and (string= stat "identifier_pos") (or (is-space (char string0 iter)) (is-parens (char string0 iter)) (is-quotes (char string0 iter)))) (setf stat "identifier"))
						((and (string= stat "identifier_error_start") (and (not (is-parens (char string0 iter))) (not (is-space (char string0 iter))))) (setf stat "identifier_error_start"))
						((and (string= stat "identifier_error_start") (is-space (char string0 iter))) (setf stat "identifier_error_end"))
						((and (string= stat "identifier_pos") (not (alphanumericp (char string0 iter)))) (setf stat "identifier_error_start"))
						((and (string= stat "identifier_pos") (alphanumericp (char string0 iter))) (setf stat "identifier_pos"))
						((and (alpha-char-p (char string0 iter)) (not (string= stat "value_pos")) (not (string= stat "zero_pos")) (not (string= stat "value_error_start")) (not (string= stat "value_error_start2"))) (setf stat "identifier_pos"))

						;checking zero and values starting with zero
						((and (not (alphanumericp (char string0 iter))) (string= stat "value_error_start")) (setf stat "value_error_end"))
						((and (alphanumericp (char string0 iter)) (string= stat "value_error_start")) (setf stat "value_error_start"))
						((and (alphanumericp (char string0 iter)) (string= stat "zero_pos")) (setf stat "value_error_start")) ;any alphanum after 0 gives error
						((string= stat "zero_pos") (setf stat "zero"))
						((and (eql (char string0 iter) #\0 ) (not (string= stat "value_pos"))) (setf stat "zero_pos"))

						;handling values
						;((and (eql (char string0 iter) #\( ) (or (string= stat "value_error_start2") (string= stat "value_pos"))) (setf stat "op"))
						;((and (eql (char string0 iter) #\) ) (or (string= stat "value_error_start2") (string= stat "value_pos"))) (setf stat "cp"))
						((and (or (is-space (char string0 iter)) (is-parens (char string0 iter))) (string= stat "value_error_start2")) (setf stat "value_error_end2"))
						((and (not (digit-char-p (char string0 iter))) (string= stat "value_error_start2") (not (is-space (char string0 iter))) (not (is-parens (char string0 iter)))) (setf stat "value_error_start2"))
						((and (string= stat "value_pos") (not (digit-char-p (char string0 iter))) (not (is-space (char string0 iter))) (not (is-parens (char string0 iter)))) (setf stat "value_error_start2"))
						((and (or (is-space (char string0 iter)) (is-parens (char string0 iter))) (string= stat "value_pos")) (setf stat "value"))
						((and (string= stat "value_pos") (digit-char-p (char string0 iter))) (setf stat "value_pos"))
						((digit-char-p (char string0 iter)) (setf stat "value_pos"))

						;handling operators
						;handling quotation marks
						((eql (char string0 iter) #\" ) (progn (setf stat "quotation") ))
						((eql (char string0 iter) #\( ) (setf stat "op"))
						((eql (char string0 iter) #\) ) (setf stat "cp"))
						((eql (char string0 iter) #\+ ) (setf stat "plus"))
						((eql (char string0 iter) #\- ) (setf stat "minus"))
						((eql (char string0 iter) #\/ ) (setf stat "div"))
						((eql (char string0 iter) #\“ ) (setf stat "oc"))
						((eql (char string0 iter) #\” ) (setf stat "cc"))
						((eql (char string0 iter) #\, ) (setf stat "comma"))
						((and (eql (char string0 iter) #\* ) (string= stat "mult_pos")) (setf stat "dbl_mult"))
						((string= stat "mult_pos") (setf stat "mult"))
						((eql (char string0 iter) #\* ) (setf stat "mult_pos"))
						;handling whitespaces
						((is-space (char string0 iter)) (progn (setf stat "end"))) 
						((not (is-space (char string0 iter))) (setf stat "start"))

						;(t (progn (tokenize-test lex0) (setf stat "start")))
						;((string= stat "end") (tokenize-test lex0))
					)
					
					(if (or (string= stat "start") (string= stat "comment_error"))
						(format stream "ERROR symbol can not be tokenized~%"))

					(if (string= stat "zero")
						(progn (format stream "VALUE~%") (setf stat "start")))

					(if (string= stat "value")
						(progn (format stream "VALUE~%") (setf stat "start")))

					

					(if (string= stat "value_error_end") ;for zero check
						(progn (format stream "ERROR the symbol is not a value~%") (setf stat "start")))

					(if (string= stat "value_error_end2") ;for other values
						(progn (format stream "ERROR the symbol is not a value~%") (setf stat "start")))

					

					(if (or (string= stat "start") (string= stat "identifier_pos"))
						(setf lex0 (concatenate 'string lex0 (string (char string0 iter))))) ;add next char to word		

					;(format stream "word: ~d stat: ~d~%" lex0 stat)

					(if (string= stat "comment_end") ;consume the whole string
						(progn (format stream "COMMENT~%") (setf stat "start")))

					(if (= iter (- (length string0) 1))
						(setf stat "start"))

					

					(tokenize-operator stat stream)
					


					(if (or (string= stat "end") (string= stat "identifier"))
						(progn (setf kwFlag (tokenize-test lex0 stream)) (tokenize-value lex0 stream) (setf lex0 "")))

					;function if above if did not print anything
					(if (and (not kwFlag) (string= stat "identifier"))
						(progn (format stream "IDENTIFIER~%") (setf stat "start")))

					(if (string= stat "identifier_error_end")
						(progn (format stream "ERROR the symbol is not an identifier~%") (setf stat "start")))

					;parens
					(if (eql (char string0 iter) #\( ) (format stream "OP_OP~%"))
					(if (eql (char string0 iter) #\) ) (format stream "OP_CP~%"))

					(if (eql (char string0 iter) #\" )
						(progn
							(incf quotCounter)
						 	(if (= (mod quotCounter 2) 1)
								(format stream "OP_OC~%")
								(format stream "OP_CC~%"))))

					;double mult
					(if (string= stat "dbl_mult")
						(format stream "OP_DBLMULT~%"))

					(if (string= stat "mult")
						(format stream "OP_MULT~%"))

					;(print iter)
				)
		)
	)
)

(defun tokenize-value (lex0 stream)
	(if (not (string= lex0 ""))
		(if   (numberp (ignore-errors (parse-integer lex0)))
			(format stream "VALUEA~%"))))

(defun tokenize-operator (stat stream)
	;(if (string= stat "op")
	;	(format t "OP_OP~%"))
	;(if (string= stat "cp")
	;	(format t "OP_CP~%"))
	(if (string= stat "plus")
		(format stream "OP_PLUS~%"))
	(if (string= stat "minus")
		(format stream "OP_MINUS~%"))
	(if (string= stat "div")
		(format stream "OP_DIV~%"))
	(if (string= stat "oc")
		(format stream "OP_OC~%"))
	(if (string= stat "cc")
		(format stream "OP_CC~%"))
	(if (string= stat "comma")
		(format stream "OP_COMMA~%")))

(defun tokenize-test (lex0 stream)
	(cond
		;keywords
		((string= lex0 "and")	(progn (format stream "KW_AND~%") (return-from tokenize-test t)))
		((string= lex0 "or")	(progn (format stream "KW_OR~%") (return-from tokenize-test t)))
		((string= lex0 "not")	(progn (format stream "KW_NOT~%") (return-from tokenize-test t)))
		((string= lex0 "equal")	(progn (format stream "KW_EQUAL~%") (return-from tokenize-test t)))
		((string= lex0 "less")	(progn (format stream "KW_LESS~%") (return-from tokenize-test t)))
		((string= lex0 "nil")	(progn (format stream "KW_NIL~%") (return-from tokenize-test t)))
		((string= lex0 "list")	(progn (format stream "KW_LIST~%") (return-from tokenize-test t)))
		((string= lex0 "append")(progn (format stream "KW_APPEND~%") (return-from tokenize-test t)))
		((string= lex0 "concat")(progn (format stream "KW_CONCAT~%") (return-from tokenize-test t)))
		((string= lex0 "set")	(progn (format stream "KW_SET~%") (return-from tokenize-test t)))
		((string= lex0 "deffun")(progn (format stream "KW_DEFFUN~%") (return-from tokenize-test t)))
		((string= lex0 "for")	(progn (format stream "KW_FOR~%") (return-from tokenize-test t)))
		((string= lex0 "if")	(progn (format stream "KW_IF~%") (return-from tokenize-test t)))
		((string= lex0 "exit")	(progn (format stream "KW_EXIT~%") (return-from tokenize-test t)))
		((string= lex0 "load")	(progn (format stream "KW_LOAD~%") (return-from tokenize-test t)))
		((string= lex0 "disp")	(progn (format stream "KW_DISP~%") (return-from tokenize-test t)))
		((string= lex0 "true")	(progn (format stream "KW_TRUE~%") (return-from tokenize-test t)))
		((string= lex0 "false")	(progn (format stream "KW_FALSE~%") (return-from tokenize-test t)))
		)
	;(if (or (not (string= lex0 "")) (not (null lex0)))
	;	(print "ERROR"))
	)

(defun repl (stream)
	(let (exit nextLine)
		
	(loop while (null exit)
		do
		(progn
			(setf nextLine (read-line))
			
			(test nextLine t)
			(test nextLine stream)
			;;(print (concatenate 'string (concatenate 'string nextLine "       ") (string #\newline)))
			(if (string= nextLine "")
				(setf exit t))))))

(defun gppinterpreter (&optional fileName)
	(with-open-file (outStream "parsed_lisp.txt" :direction :output)
	(let (fileString)

	(if (not (null *args*))
		(progn
			(setf fileString (file-to-string (string (nth 0 *args*))))
			;(print fileString)
			(test fileString outStream))
		(repl outStream)))))

;starting program
(gppinterpreter)
