;;;; QuantumLisp.asd

(asdf:defsystem #:QuantumLisp
	:description "Quantum computing simulator"
	:author "fwcd"
	:license  "MIT"
	:version "0.0.1"
	:serial t
	:components (
		(:file "package")
		(:file "src/Utils")
		(:file "src/RandomMath")
		(:file "src/BitMath")
		(:file "src/MatrixMath")
		(:file "src/QuantumLisp")
		(:file "src/QuantumGates")
	)
)
