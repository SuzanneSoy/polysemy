#lang racket/base

(require racket/match
         (for-syntax racket/base
                     "utils.rkt"))

(provide
 ;; The only polysemic id (all others are renamings of this one)
 the-polysemic-id
 ;; The only safeguard id (all others are renamings of this one)
 the-safeguard-id)

;; We can have a safeguard identifier to detect uses of rename-in, rename-out
;; and only-in, instead of their poly- counterparts. The safeguard
;; identifier does not do anything, but should always be available. If it is not
;; available it means that some unprotected renaming occurred, and an error is
;; thrown.
(define-syntax the-safeguard-id
  (λ (stx)
    (raise-syntax-error 'safeguard "Invalid use of internal identifier" stx)))

;; Shorthand for syntax-local-value
(define-for-syntax (maybe-slv id) (syntax-local-value id (λ () #f)))

;; Creates a wrapper for a prop:…, by extracting the the given `meaning`
;; for the identifier.
(define-for-syntax ((make-wrapper meaning fallback-id fallback-app) stx)
  (syntax-case stx ()
    [(self . rest)
     (let ([slv (maybe-slv (gen-id/check #'self meaning))])
       (if slv
           (slv stx)
           (fallback-app stx #'self #'rest)))]
    [self
     (identifier? #'self)
     (let ([slv (maybe-slv (gen-id/check #'self meaning))])
       (if slv
           (slv stx)
           (fallback-id stx)))]
    [_
     (raise-syntax-error 'polysemic-identifier
                         "illegal use of polysemic identifier"
                         stx)]))

;; Wrapper for prop:procedure on a transformer id.
;; Dispatches to 
(define-for-syntax (macro-wrapper _self stx)
  (syntax-case stx (set!)
    [(set! v . _)
     (let ([slv (maybe-slv (gen-id/check #'v 'set!-macro))])
       (if slv
           (slv stx)
           (raise-syntax-error
            'set!
            (format "Assignment with set! is not allowed for ~a"
                    (syntax->datum #'v))
            stx)))]
    [(self . rest)
     (let ([slv (maybe-slv (gen-id/check #'self 'normal-macro))])
       (if slv
           (slv stx)
           (datum->syntax
            stx
            `((,(datum->syntax #'self '#%top #'self #'self) . ,#'self)
              . ,#'rest)
            stx
            stx)))]
    [x
     (identifier? #'x)
     (begin
       (let ([slv (maybe-slv (gen-id/check #'x 'identifier-macro))])
         (if slv
             (slv stx)
             (datum->syntax stx `(#%top . ,#'x) stx stx))))]
    [_
     (raise-syntax-error 'polysemic-identifier
                         "illegal use of polysemic identifier"
                         stx)]))

;; An instance of this struct are bound (as transformer values) to the (only)
;; polysemic id.
(begin-for-syntax
  (struct polysemic ()
    #:property prop:match-expander
    (make-wrapper 'match-expander
                  (λ (id) #`(var #,id))
                  (λ (stx id args) (datum->syntax stx `(,id . ,args) stx stx)))
    #:property prop:procedure macro-wrapper))

;; The only polysemic id (all others are renamings of this one)
(define-syntax the-polysemic-id (polysemic))
