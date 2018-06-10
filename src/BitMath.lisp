;;;; BitMath.lisp

(in-package #:QuantumLisp)

; Source: http://tomszilagyi.github.io/2016/01/CL-bitwise-Rosettacode

; Bitshifts the number x left
;
; x - number (the number)
; width - number (the total amount of integer bits)
; bits - number (the amount of bits shifted)
;
; Returns a number
(defun bitshift-left(x width bits)
	(logand (ash x bits) (1- (ash 1 width)))
)

; Bitshifts the number x right
;
; x - number (the number)
; width - number (the total amount of integer bits)
; bits - number (the amount of bits shifted)
;
; Returns a number
(defun bitshift-right(x width bits)
	(logand (ash x (- bits)) (1- (ash 1 width)))
)

; Converts a list of bits to a number represented by the
; corresponding bits
;
; bitlist - list of numbers (zeroes and ones)
;
; Returns a number
(defun bitlist-to-int(bitlist)
	(reduce (lambda (current previous) (+ (* current 2) previous)) bitlist)
)

; Converts a number to a binary representation
; Source: https://stackoverflow.com/questions/22668217/decimal-to-binary-in-lisp-make-a-non-nested-list
;
; n - number (an integer)
;
; Returns a list of numbers (zeroes and ones)
(defun int-to-bitlist(n)
	(cond
		((= n 0) (list 0))
		((= n 1) (list 1))
		(t (nconc (int-to-bitlist (truncate n 2)) (list (mod n 2))))
	)
)
