;;;; Huffman Code Calculator

;;; Reads a file, calculates huffman codes
;;; outputs the codes to a file

;; Author: Rahmet Ali Olmez 
;; October 2020

;represents the nodes of the huffman tree
(defstruct huffman-node
	ch
	freq
	code
	left
	right)

;reads given file and returns it as a string
(defun file-to-string (fileName)
	(let (text)
  		(with-open-file (inStream fileName)
    		(setf text (make-string (file-length inStream)))
    	  	(read-sequence text inStream))
    	  (return-from file-to-string text)))

;searches char in list of huffman nodes
;increases freq by 1 if found
;puts new huffman node with this char in list with frequency of 1 if not found
(defun check-char-in-list (ch0 list0)
	(loop for node in list0
		do
			(progn (if (eql ch0 (huffman-node-ch node))
						(progn (incf (huffman-node-freq node))
							(return-from check-char-in-list list0)))))
	(setf list0 (append list0 (list (make-huffman-node :ch ch0 :freq 1))))
	(return-from check-char-in-list list0))

;creates nodes for every unique character
(defun create-huffman-list (text)
	(let (newList)
		(loop for nextChar across text 
			do 
				(if (null newList)
					(setf newList (append newList (list (make-huffman-node :ch nextChar :freq 1))))
					(setf newList (check-char-in-list nextChar newList))))	
		(return-from create-huffman-list newList)))

;sorts list according to frequency of characters
(defun sort-huffman-list (list0)
	(sort list0 #'< :key #'huffman-node-freq))

;sorts list according to length of huffman codes
(defun sort-huffman-codes (list0)
	(sort list0 #'< :key (lambda (p) (length (huffman-node-code p)))))

(defun print-huffman-list (list0)
	(loop for node in list0
		do
			(progn (write node)
					(terpri))))

;as a result of this function
;there remains one node in the list
;which is the root of the tree
(defun create-huffman-tree (list0)
	(if (< (length list0) 2)
		(return-from create-huffman-tree list0))
	(let (newNode)
		(setf newNode (make-huffman-node))
		(setf (huffman-node-left newNode) (car list0))
		(setf list0 (cdr list0))
		(setf (huffman-node-right newNode) (car list0))
		(setf list0 (cdr list0))
		(setf (huffman-node-freq newNode) 
			(+ (huffman-node-freq (huffman-node-left newNode)) 
				(huffman-node-freq (huffman-node-right newNode))))
		(setf list0 (cons newNode list0))
		(sort-huffman-list list0))
	(create-huffman-tree list0))

(defun is-leaf (node0)
	(if (and (null (huffman-node-left node0)) (null (huffman-node-right node0)))
		(return-from is-leaf t)
		(return-from is-leaf nil)))

(defun generate-huffman-codes (node0 string0)
	(if (is-leaf node0)
			(progn 
				(setf (huffman-node-code node0) (concatenate 'string (huffman-node-code node0) string0))
				(return-from generate-huffman-codes 0)))
	
	(generate-huffman-codes (huffman-node-left node0) (concatenate 'string string0 "0"))
	
	(generate-huffman-codes (huffman-node-right node0) (concatenate 'string string0 "1")))

(defun write-file-huffman-codes (list0 fileName)
	(with-open-file (outStream fileName :direction :output)
		(loop for node in list0
			do
				(progn (format outStream "~c: ~d~%" (huffman-node-ch node) (huffman-node-code node))))))

;putting the tree leaves to a list
(defun huffman-tree-to-list (node0 list0)
	(if (is-leaf node0)
			(progn 
				(setf list0 (append list0 (list node0)))
				(return-from huffman-tree-to-list list0)))
	
	(setf list0 (huffman-tree-to-list (huffman-node-left node0) list0))
	
	(setf list0 (huffman-tree-to-list (huffman-node-right node0) list0)))

(defun main ()
	(let (huffmanList text string0 codeList) 				
		(setf text (file-to-string "paragraph.txt"))		
		(setf huffmanList (create-huffman-list text))
		(sort-huffman-list huffmanList)
		(setf huffmanList (create-huffman-tree huffmanList))
		(setf string0 "")
		(generate-huffman-codes (car huffmanList) string0)
		(setf codeList (huffman-tree-to-list (car huffmanList) codeList))
		(sort-huffman-codes codeList)
		(write-file-huffman-codes codeList "huffman_codes.txt")))

;running the program
(main)