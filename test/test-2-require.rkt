#lang racket

(require polysemy
         rackunit
         syntax/macro-testing
         (poly-rename-in "test-2-provide.rkt"
                         [[foo bar] |(poly-case string?)|]
                         [[bar foo] |(poly-case string?)|]))

(check-equal? (foo 1) 11)
(check-equal? (foo "abc") "bar-abc")
(check-equal? (bar 1) 21)
(check-equal? (bar "abc") 3)
(check-exn #px"overlap"
           (λ ()
             (convert-compile-time-error
              (baz "abc"))))