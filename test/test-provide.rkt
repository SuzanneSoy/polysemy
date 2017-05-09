#lang racket

(require polysemy
         (for-syntax syntax/parse))

(provide (poly-out [foo identifier-macro
                        my-macro-foo-token
                        my-macro2-foo-token])
         my-macro
         my-macro2)

(define-poly foo)
(define-poly foo identifier-macro (λ (stx) #'"originally foo"))

(define-poly-literal foo my-macro-foo-token my-macro-foo-token)
(define-syntax my-macro
  (syntax-parser
    [(_ a ... :my-macro-foo-token b ...)
     #''((a ...) (b ...))]))

(define-poly foo my-macro2-foo-token #'42)
(define-syntax my-macro2
  (syntax-parser
    [(_ a ... {~poly x my-macro2-foo-token} b ...)
     #''((a ...) x.value (b ...))]))
