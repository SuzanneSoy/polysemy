#lang racket

;; Baz is a chimera created by mixing foo's identifier macro and bar's
;; match expander. Note that performing a plain rename-in on a polysemic
;; identifier would be a recipe for disaster (it would try to access meanings
;; based on its new name, instead of accessing meanings based on its former
;; name).

(require rackunit)

(require "test-require.rkt")

(check-equal? baz "originally foo")

(check-match "originally bar match-expander" (baz))

(check-equal? (match "something else"
                [(baz) 'bad]
                [_ 'ok])
              'ok)