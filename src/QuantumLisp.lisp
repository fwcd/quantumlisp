;;;; QuantumLisp.lisp

(in-package #:QuantumLisp)

; Creates a superposition of the given
; bits. The result is a vector of complex
; numbers representing the probabilities
; of each possible outcome.
;
; bits - list of numbers (zeroes and ones)
;
; Returns a list of complex numbers
(defun superpos(bits)
	(if (= (list-length bits) 1)
		(let ((singleBit (nth 0 bits)))
			(cond
				((= singleBit 0) (list 1 0))
				((= singleBit 1) (list 0 1))
			)
		)
		; else (there is more than one bit)
		(generate-list
			(let ((bitsAsInt (bitlist-to-int bits)))
				(lambda (index)
					; The classical bit combination has the
					; probability 1 and all other combinations 0
					(if (= index bitsAsInt) 1 0)
				)
			)
			(expt 2 (list-length bits)) ; There are 2^n possible qubit combinations
		)
	)
)

; Prints a superposition to stdout
;
; superposition - list of complex numbers
(defun print-superpos(superposition)
	(let ((index 0))
		(dolist (probability superposition)
			(let ((bits (int-to-bitlist index)) (absProbability (abs probability)))
				(print (format nil "~{~a~}" (list bits " => " absProbability)))
			)
			(incf index)
		)
	)
)

; Collapses a superposition to a series
; of classical bits
;
; superposition - list of complex numbers
;
; Returns a list of numbers (zeroes and ones)
(defun collapse(superposition)
	(int-to-bitlist (funcall (biased-random
		(generate-list (lambda (i) i) (list-length superposition))
		(mapcar (lambda (complexProbability) (abs complexProbability)) superposition)
	)))
)
