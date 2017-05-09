#lang racket

(require polysemy
         rackunit)

(provide (all-defined-out))

(define-poly foo)
(define-poly foo match-expander (λ (stx) #'"originally foo"))
(define-poly-case (foo [v integer?]) (+ v 10))
(define-poly-case (foo [v string?]) (string-length v))

(define-poly bar)
(define-poly-case (bar [v integer?]) (+ v 20))
(define-poly-case (bar [v string?]) (string-append "bar-" v))

(define-poly baz)
(define-poly-case (baz [v integer?]) (+ v 20))
(define-poly-case (baz [v number?]) (+ v 20))
(define-poly-case (baz [v string?]) (string-append "baz-" v))

(check-equal? (foo 1) 11)
(check-equal? (foo "abc") 3)
(check-equal? (bar 1) 21)
(check-equal? (bar "abc") "bar-abc")