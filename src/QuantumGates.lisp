;;;; QuantumGates.lisp

(in-package #:QuantumLisp)

; TODO: Currently the quantum gates only operate on
; superpositions with exactly as many qubits as needed.
; Instead, the caller should be able to choose which
; qubits from a (possibly entangled) superposition
; should be affected. This would involve modifying
; the matrices using the tensor product.

; Applies a quantum gate to the superposition
(defun apply-qgate(superposition gateMatrix)
	(matrix-to-vec (matrix-multiply gateMatrix (vec-to-matrix superposition)))
)

; Applies the identity quantum gate to the superposition
(defun identity-gate(superposition)
	(apply-qgate superposition (list
		'(1 0)
		'(0 1)
	))
)

; Applies the Pauli-X (also known as NOT) quantum gate to the superposition
(defun pauli-x(superposition)
	(apply-qgate superposition (list
		'(0 1)
		'(1 0)
	))
)

; Applies the Pauli-Y quantum gate to the superposition
(defun pauli-y(superposition)
	(apply-qgate superposition (list
		'(0 #C(0 -1))
		'(#C(1 0) 0)
	))
)

; Applies the Pauli-Z quantum gate to the superposition
(defun pauli-z(superposition)
	(apply-qgate superposition (list
		'(1 0)
		'(0 -1)
	))
)

; Applies the hadamard quantum gate to the superposition
(defun hadamard(superposition)
	(apply-qgate superposition (matrix-scale 0.70710678118 (list ; 0.707... is the inverse square root of 2
		'(1 1)
		'(1 -1)
	)))
)

; Applies the CNOT quantum gate to the superposition
(defun cnot(superposition)
	(apply-qgate superposition (list
		'(1 0 0 0)
		'(0 1 0 0)
		'(0 0 0 1)
		'(0 0 1 0)
	))
)

; Applies the swap quantum gate to the superposition
(defun swap(superposition)
	(apply-qgate superposition (list
		'(1 0 0 0)
		'(0 0 1 0)
		'(0 1 0 0)
		'(0 0 0 1)
	))
)

; Applies the sqrt-swap quantum gate to the superposition
(defun sqrt-swap(superposition)
	(apply-qgate superposition (list
		'(1 0 0 0)
		'(0 #C(1/2 1/2) #C(1/2 -1/2) 0)
		'(0 #C(1/2 -1/2) #C(1/2 1/2) 0)
		'(0 0 0 1)
	))
)

; Applies the CCNOT (also known as Toffoli) quantum gate to the superposition
(defun ccnot(superposition)
	(apply-qgate superposition (list
		'(1 0 0 0 0 0 0 0)
		'(0 1 0 0 0 0 0 0)
		'(0 0 1 0 0 0 0 0)
		'(0 0 0 1 0 0 0 0)
		'(0 0 0 0 1 0 0 0)
		'(0 0 0 0 0 1 0 0)
		'(0 0 0 0 0 0 0 1)
		'(0 0 0 0 0 0 1 0)
	))
)

; Applies the CSWAP (also known as Fredkin) quantum gate to the superposition
(defun cswap(superposition)
	(apply-qgate superposition (list
		'(1 0 0 0 0 0 0 0)
		'(0 1 0 0 0 0 0 0)
		'(0 0 1 0 0 0 0 0)
		'(0 0 0 1 0 0 0 0)
		'(0 0 0 0 1 0 0 0)
		'(0 0 0 0 0 0 1 0)
		'(0 0 0 0 0 1 0 0)
		'(0 0 0 0 0 0 0 1)
	))
)
