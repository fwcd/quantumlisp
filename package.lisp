;;;; package.lisp

(defpackage #:QuantumLisp
	(:use #:cl)
	(:export
		:matrix-create
		:superpos
		:print-superpos
		:collapse
		:identity-gate
		:pauli-x
		:pauli-y
		:pauli-z
		:hadamard
		:cnot
		:swap
		:sqrt-swap
		:ccnot
		:cswap
	)
)
