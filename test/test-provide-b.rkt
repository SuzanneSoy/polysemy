#lang racket

(require polysemy)

(provide (poly-out [foo match-expander]
                   [bar match-expander identifier-macro]))

(define-poly foo match-expander (λ (stx) #'"originally foo match-expander"))

(define-poly bar)
(define-poly bar match-expander (λ (stx) #'"originally bar match-expander"))
(define-poly bar identifier-macro (λ (stx) #'"originally bar"))
