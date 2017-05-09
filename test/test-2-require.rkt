#lang racket

(require polysemy
         rackunit
         (poly-rename-in "test-2-provide.rkt"
                         [foo |(poly-case string?)| bar]
                         [bar |(poly-case string?)| foo]))

(check-equal? (foo 1) 11)
(check-equal? (foo "abc") "bar-abc")
(check-equal? (bar 1) 21)
(check-equal? (bar "abc") 3)
(baz "abc")