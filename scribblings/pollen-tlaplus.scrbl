#lang scribble/manual
@require[@for-label[pollen-tlaplus
                    racket/base]]

@title{pollen-tlaplus}
@author[(author+email "Nick Gunn" "nick@ausimian.net")]

@defmodule[pollen-tlaplus]

A set of function to turn ASCII-formatted blocks of @link["https://lamport.azurewebsites.net/tla/tla.html"]{TLA⁺} into @link["https://docs.racket-lang.org/pollen/"]{@italic{Pollen}} x-expressions.

@defproc[
(tla [elem any/c] ...) any/c]
Produces a pretty-printed rendering of the supplied input.
