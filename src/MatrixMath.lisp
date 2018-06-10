;;;; MatrixMath.lisp

(in-package #:QuantumLisp)

; Calculates the dot product of two vectors
;
; vecA - list of numbers
; vecB - list of numbers
;
; Returns another list of numbers
(defun dot(vecA vecB)
	(reduce #'+ (mapcar #'* vecA vecB))
)

; Converts a (column) vector to a matrix
;
; vector - list of numbers
;
; Returns a list of lists (rows) of numbers
(defun vec-to-matrix(vector)
	(mapcar (lambda (item) (list item)) vector)
)

; Converts a matrix to a (column) vector
;
; matrix - list of lists (rows) of numbers
;
; Returns a list numbers
(defun matrix-to-vec(matrix)
	(mapcar (lambda (item) (nth 0 item)) matrix)
)

; Returns the nth row of a matrix as a list
;
; index - number
; matrix - list of lists (rows) of numbers
;
; Returns a list of numbers
(defun matrix-row(index matrix)
	(nth index matrix)
)

; Returns the nth column of a matrix as a list
;
; index - number
; matrix - list of lists (rows) of numbers
;
; Returns a list of numbers
(defun matrix-column(index matrix)
	(mapcar (lambda (row) (nth index row)) matrix)
)

; Returns the number of rows in this matrix
;
; matrix - list of lists (rows) of numbers
;
; Returns a number
(defun matrix-row-count(matrix)
	(list-length matrix)
)

; Returns the number of columns in this matrix,
; assuming the matrix is rectangular (and thus
; valid)
;
; matrix - list of lists (rows) of numbers
;
; Returns a number
(defun matrix-column-count(matrix)
	(list-length (nth 0 matrix))
)

; Creates a new matrix using a generator function
;
; generator - function that takes a row index (number) and a column index (number) and returns something (the matrix entry)
; rows - number
; cols - number
;
; Returns a list of lists (rows) of numbers
(defun matrix-create(generator rows cols)
	(generate-list (lambda (rowIndex)
		(generate-list (lambda (colIndex)
			(funcall generator rowIndex colIndex)
		) cols)
	) rows)
)

; Performs matrix multiplication with two matrices
;
; matLeft - list of lists (rows) of numbers
; matRight - list of lists (rows) of numbers
;
; Returns a list of lists (rows) of numbers
(defun matrix-multiply(matLeft matRight)
	(matrix-create
		(lambda (row col)
			(dot (matrix-row row matLeft) (matrix-column col matRight))
		)
		(matrix-row-count matLeft)
		(matrix-column-count matRight)
	)
)

; Maps the matrix components elementwise
;
; mapper - function that takes a number and returns a number
; matrix - list of lists (rows) of numbers
;
; Returns a list of lists (rows) of numbers
(defun mapcar-matrix(mapper matrix)
	(mapcar (lambda (row)
		(mapcar (lambda (cell)
			(funcall mapper cell)
		) row)
	) matrix)
)

; Multiplies the matrix (elementwise) with a scalar
;
; scalar - a number
; matrix - list of lists (rows) of numbers
;
; Returns a list of lists (rows) of numbers
(defun matrix-scale(scalar matrix)
	(mapcar-matrix (lambda (value) (* scalar value)) matrix)
)
