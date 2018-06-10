;;;; Utils.lisp

(in-package #:QuantumLisp)

; Constructs a list of the given lenght
; using a generator function
;
; generator - function that takes an index (number) and returns something
; length - number
;
; Returns a list
(defun generate-list(generator length)
	(let ((resultingList (list)))
		(dotimes (index length)
			(setq resultingList (append resultingList (list
				(funcall generator index)
			)))
		)
		resultingList
	)
)

; Maps a list using a mapper that accepts an
; additional index parameter.
;
; mapper - function that takes an item (from the list) and a number (the index) and returns something
; items - a list of arbitrary items
;
; Returns another list
(defun mapcar-with-index(mapper items)
	(let ((index -1))
		(mapcar (lambda (item)
			(incf index)
			(funcall mapper item index)
		) items)
	)
)
