#lang racket/base

(require racket/base
         racket/contract
         racket/string)

(provide gen-id
         gen-id/check)

;; Utilities
;; _____________________________________________________________________________

;; Escapes the identifier, so that it does not contain the separator character
(define/contract (escape-symbol sym separator escape)
  (-> symbol? char? char? string?)
  (let ()
    (define s1 (symbol->string sym))
    (define s2 (string-replace s1
                               (format "~a" escape)
                               (format "~a~a" escape escape)))
    (define s3 (string-replace s1
                               (format "~a" separator)
                               (format "~a~a" escape separator)))
    s3))

;; Generates a single-meaning identifier from `id` and `meaning`, possibly
;; escaping some characters in `meaning` to remove ambiguities.
(define/contract (gen-id id meaning)
  (-> identifier? symbol? identifier?)
  (let ()
    (define s (format " polysemy ~a ~a "
                      (escape-symbol meaning #\space #\\)
                      (symbol->string (syntax-e id))))
    (datum->syntax id (string->symbol s) id id)))

(define/contract (gen-id/check id meaning)
  (-> identifier? symbol? identifier?)
  (unless (syntax-local-value (gen-id id '| safeguard |) (λ () #f))
    (raise-syntax-error
     'polysemy
     (format
      (string-append
       ;; TODO: check guidelines for error messages.
       "the safeguard for ~a was not found."
       " Usually, this means that only-in, rename-in or rename-out were used"
       " instead of their poly-rename-in, poly-only-in, or poly-out"
       " counterparts.")
      (syntax-e id))
     id))
  (gen-id id meaning))