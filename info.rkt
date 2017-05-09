#lang info
(define collection "polysemy")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/polysemy.scrbl" ())))
(define pkg-desc
  "Polysemic identifiers, each meaning can be required and renamed separately")
(define version "0.1")
(define pkg-authors '("Georges Dupéron"))
