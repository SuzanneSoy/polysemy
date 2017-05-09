#lang racket

;; Test without requiring polysemy

(require rackunit)

(require "test-provide.rkt"
         "test-provide-b.rkt")

(check-equal? foo "originally foo")
(check-equal? bar "originally bar")

(check-match "originally foo match-expander" (foo))

(check-equal? (match "something else"
                [(foo) 'bad]
                [_ 'ok])
              'ok)
