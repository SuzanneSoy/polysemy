#lang racket

;; Baz is a chimera created by mixing foo's identifier macro and bar's
;; match expander. Note that performing a plain rename-in on a polysemic
;; identifier would be a recipe for disaster (it would try to access meanings
;; based on its new name, instead of accessing meanings based on its former
;; name).

(require rackunit
         syntax/macro-testing)

(require (rename-in "test-require.rkt" [baz fuzz]))

(check-exn #px"safeguard"
           (λ () (convert-compile-time-error fuzz)))