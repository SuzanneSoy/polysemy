#lang racket

(require polysemy
         rackunit)

(provide (poly-out [baz identifier-macro match-expander]))

(require (poly-rename-in "test-provide.rkt"
                         [(foo baz) identifier-macro])
         (poly-rename-in "test-provide-b.rkt"
                         [(bar foo) identifier-macro]
                         [(bar baz) match-expander]
                         [foo match-expander]))

(define-poly bar identifier-macro (λ (stx) #'"overridden bar"))

(check-equal? foo "originally bar")
(check-equal? bar "overridden bar")
(check-equal? baz "originally foo")

(check-match "originally foo match-expander" (foo))

(check-equal? (match "something else"
                [(foo) 'bad]
                [_ 'ok])
              'ok)

(check-match "originally bar match-expander" (baz))

(check-equal? (match "something else"
                [(baz) 'bad]
                [_ 'ok])
              'ok)

(check-equal? (my-macro a aa aaa foo b bb bbb)
              '((a aa aaa) (b bb bbb)))

(check-equal? (my-macro2 a aa aaa foo b bb bbb)
              '((a aa aaa) 42 (b bb bbb)))
