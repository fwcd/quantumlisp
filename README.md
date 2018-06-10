# QuantumLisp
Quantum computing simulator for Common Lisp.

## Examples
Creating and printing a superposition:
```lisp
(defvar qubits (quantumlisp:superpos (list 1 0)))
(quantumlisp:print-superpos qubits)
```

Applying different quantum gates to a superposition:
```lisp
(setq qubits (quantumlisp:swap qubits))
(setq qubits (quantumlisp:sqrt-swap qubits))
(setq qubits (quantumlisp:cnot qubits))
```

Collapsing a superposition:
```lisp
(quantumlisp:collapse qubits)
```

## Getting Started
* An installation of the Quicklisp package manager is required
* (push #p"/YOUR/CLONE/PATH/QuantumLisp/" asdf:*central-registry*)
* (ql:quickload "quantumlisp")

## License
MIT
