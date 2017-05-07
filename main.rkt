#lang racket/base

(provide
 ;;; Require transformer (does not work correctly, for now)
 #;poly-in
 ;; Another require transformer
 poly-rename-in
 ;; Alternative require form which handles polysemic ids
 poly-require
 ;; Definition of a polysemic id, and of a part of a polysemic id
 define-poly)

(require racket/match
         (for-syntax racket/base
                     racket/contract
                     racket/string
                     racket/require-transform
                     syntax/parse))

;; This scope is used to hide and later identify parts of polysemic identifiers.
;; Each part is stored in a separate identifier.
(define-for-syntax poly-scope (make-syntax-introducer))

;; Utilities
;; _____________________________________________________________________________

;; Escapes the identifier, so that it does not contain the separator character
(begin-for-syntax
  (define/contract (escape-symbol sym separator escape)
    (-> symbol? char? char? string?)
    (let ()
      (define s1 (symbol->string sym))
      (define s2 (string-replace s1
                                 (format "~a" escape)
                                 (format "~a~a" escape escape)))
      (define s3 (string-replace s1
                                 (format "~a" separator)
                                 (format "~a~a" separator escape)))
      s3)))

;; Generates a single-meaning identifier from `id` and `meaning`, possibly
;; escaping some characters in `meaning` to remove ambiguities.
(begin-for-syntax
  (define/contract (gen-id ctx meaning id)
    (-> syntax? symbol? identifier? identifier?)
    (let ()
      (define s (format " polysemy_~a_~a"
                        (escape-symbol meaning #\_ #\\)
                        (symbol->string (syntax-e id))))
      (datum->syntax ctx (string->symbol s) id id))))

;; Require transformer
;; _____________________________________________________________________________

;; Require transformer which allows selecting and renaming parts of polysemic
;; parts of identifiers.
#;(define-syntax poly-in
    (make-require-transformer
     (λ (stx)
       (syntax-case stx ()
         [(_ mod id ...)
          (let ()
            ;; Works, but we cannot bind a syntax transformer that way.
            (define idd (syntax-local-lift-expression #'42))
            ;; Too late, top-level uses of macros have already been prefixed
            ;; with #%app:
            (syntax-local-lift-module-end-declaration
             #'(begin (define-syntax id (λ (stx) #`'(#,stx 42))) ...))
            ;; Won't work because we have to run expand-import before the
            ;; module has a chance to be injected:
            (syntax-local-lift-module
             #'(module m racket/base
                 (provide id ...)
                 (define-syntax id (λ (stx) #`'(#,stx 42))) ...))
            (define-values (a b) (expand-import #'(only-in mod id ...)))
            (define a*
              (let ([local-id (import-local-id (car a))]
                    [src-sym (import-src-sym (car a))]
                    [src-mod-path (import-src-mod-path (car a))]
                    [mode (import-mode (car a))]
                    [req-mode (import-req-mode (car a))]
                    [orig-mode (import-orig-mode (car a))]
                    [orig-stx (import-orig-stx (car a))])
                (list (import idd
                              src-sym
                              src-mod-path
                              mode
                              req-mode
                              orig-mode
                              orig-stx))))
            (values a* b))]))))

(define-syntax poly-rename-in
  (make-require-transformer
   (syntax-parser
     [(_ mod [old-id:id meaning:id new-id:id] ...)
      (with-syntax ([(old-generated-id ...)
                     (map gen-id
                          (syntax->list #'(old-id ...))
                          (map syntax-e (syntax->list #'(meaning ...)))
                          (syntax->list #'(old-id ...)))]
                    [(new-generated-id ...)
                     (map gen-id
                          (syntax->list #'(new-id ...))
                          (map syntax-e (syntax->list #'(meaning ...)))
                          (syntax->list #'(new-id ...)))])
        (expand-import
         #'(rename-in mod [old-generated-id new-generated-id] ...)))])))

;; polysemic require (experiment, nothing interesting for now)
(define-syntax poly-require
  (λ (stx)
    (syntax-case stx ()
      [(_ mod id ...)
       (with-syntax ([(tmp ...) (generate-temporaries #'(id ...))])
         #'(begin
             (require (only-in mod [id tmp] ...))
             (define-syntax id (λ (stx) #'42))
             ...))])))

;; Definition of polysemic identifiers and parts of these
;; _____________________________________________________________________________

;; Definition of a new polysemic identifier
(define-syntax (define-poly stx)
  (syntax-case stx ()
    [(_ id)
     #'(define-syntax id (polysemic #'id))]
    [(_ id meaning value)
     (with-syntax ([generated-id (gen-id #'id (syntax-e #'meaning) #'id)])
       #'(define-syntax generated-id value))]))

;; Creates a wrapper for a prop:…, by extracting the the given `meaning`
;; for the identifier.
(define-for-syntax ((make-wrapper meaning) self stx)
  ((syntax-local-value (gen-id (car (syntax-e stx)) meaning (polysemic-id self))) stx))

;; Wrapper for prop:procedure on a transformer id.
;; Dispatches to 
(define-for-syntax (macro-wrapper self stx)
  (define id (polysemic-id self))
  (if (syntax? stx)
      (syntax-case stx (set!)
        [x
         (identifier? #'x)
         ((syntax-local-value (gen-id #'x 'identifier-macro id)) stx)]
        [(set! v . _)
         ((syntax-local-value (gen-id #'v 'set!-macro id)) stx)]
        [(self . _)
         ((syntax-local-value (gen-id #'self 'normal-macro id)) stx)])
      (error "oops")#;((syntax-local-value (gen-id 'normal-macro id)) stx)))

;; Instances of this struct are bound (as transformer values) to polysemic ids.
(begin-for-syntax
  (struct polysemic (id)
    #:property prop:match-expander (make-wrapper 'match-expander)
    #:property prop:procedure macro-wrapper))