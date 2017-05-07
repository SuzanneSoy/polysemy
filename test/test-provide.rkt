#lang racket

(require polysemy)

(provide (all-defined-out))

(define-poly foo)
(define-poly foo identifier-macro (λ (stx) #'"originally foo"))

(define-poly bar)
(define-poly bar identifier-macro (λ (stx) #'"originally bar"))

(define-poly baz)