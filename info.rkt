#lang info
(define collection "pollen-tlaplus")
(define deps '("base" "megaparsack" "functional-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/pollen-tlaplus.scrbl" ())))
(define pkg-desc "Pollen tag functions to convert TLA+ to HTML.")
(define version "0.1")
