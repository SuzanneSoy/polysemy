#lang racket

(require polysemy)

;(require (poly-in "test-provide.rkt" foo))
;(poly-require "test-provide.rkt" foo)

(require (poly-rename-in "test-provide.rkt"
                         [foo identifier-macro baz]
                         [bar identifier-macro foo]))

(define-poly bar identifier-macro (λ (stx) #'"overridden bar"))

foo ;; "originally bar"
bar ;; "overridden bar"
baz ;; "originally foo"

