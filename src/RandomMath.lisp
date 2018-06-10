;;;; RandomMath.lisp

(in-package #:QuantumLisp)

; Source: https://stackoverflow.com/questions/29359922/lisp-biased-number-generator

; Stochastically/"randomly" picks a number
; from a weighted probability distribution.
;
; values - list (all possible values)
; weights - list of numbers (their corresponding probabilities)
;
; Returns a value
(defun biased-random(values weights)
	(multiple-value-bind (total values)
		(loop for v in values
			for w in (mapcar (lambda (rawWeight) (round (* rawWeight 128))) weights)
			nconc (make-list w :initial-element v) into vs
			sum w into total
			finally (return (values total (coerce vs 'vector)))
		)
		(lambda () (aref values (random total)))
	)
)
