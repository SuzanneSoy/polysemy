#lang racket/base

(require racket/match
         (for-syntax racket/base
                     racket/contract
                     racket/set
                     syntax/id-table
                     syntax/id-set
                     "utils.rkt"))

(provide
 ;; The only polysemic id (all others are renamings of this one)
 the-polysemic-id
 ;; The only safeguard id (all others are renamings of this one)
 the-safeguard-id
 ;; The only case-dispatch macro (all others are renamings of this one)
 the-case-dispatch
 ;; Records all known meanings
 (for-syntax all-meanings
             register-meanings))
(begin-for-syntax
  (provide
   ;; Represents a single overload of a function
   (struct-out a-case)))

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

;; Record all known meanigns, so that the-case-dispatch-impl can perform some
;; sanity checks.
(begin-for-syntax
  (define/contract all-meanings (set/c symbol? #:kind 'mutable) (mutable-set))
  (define/contract (register-meanings-end syms)
    (-> (listof symbol?) void?)
    (for ([meaning (in-list syms)])
      (set-add! all-meanings meaning)))
  
  (define/contract (register-meanings syms)
    (-> (listof symbol?) void?)
    (for ([meaning (in-list syms)])
      (set-add! all-meanings meaning))
    (syntax-local-lift-module-end-declaration
     #`(begin-for-syntax
         (register-meanings-end '#,syms)))))

(begin-for-syntax
  ;; Represents a single overload of a function (function-id + predicate-id)
  (struct a-case (f-id pred-id) #:transparent))

;; (FreeIdTable Id (Listof Id))
(define-for-syntax contracts-supertypes #f)
;; (FreeIdTable Id (Listof Id))
(define-for-syntax contracts-expand #f)
(define-for-syntax (detect-overlap stx pred-ids)
  ;; Lazily fill in the supertypes hash table, to avoid compile-time costs
  ;; when the module is later required.
  (unless contracts-supertypes
    (set! contracts-supertypes
          (make-free-id-table
           `((,#'any/c . ())
             (,#'string? . (,#'any/c))
             (,#'exact-positive-integer? . (,#'exact-integer? ,#'positive?))
             (,#'exact-integer . (,#'integer? ,#'exact?))
             (,#'integer? . (,#'number?))
             (,#'exact? . (,#'number?)) ;; not quite right
             (,#'number? . (,#'any/c))
             (,#'zero? . (,#'integer?))
             #;…))))
  ;; Lazily fill in the "expansion" hash table, to avoid compile-time costs
  ;; when the module is later required.
  (unless contracts-expand
    (set! contracts-expand
          (make-free-id-table
           `((,#'exact-nonnegative-integer? . (,#'zero?
                                               ,#'exact-positive-integer?))
             #;…))))
  ;; Build the set of covered contracts. When a contract is a union of two
  ;; disjoint contracts, it is replaced by these
  ;; (e.g. exact-nonnegative-integer? is replaced by zero? and
  ;; exact-positive-integer?)
  (define covered-ids (mutable-free-id-set))
  (for/list ([pred-id (in-list pred-ids)])
    (define expanded*
      (free-id-table-ref contracts-expand
                         pred-id
                         (λ () (list pred-id))))
    (for ([expanded (in-list expanded*)])
      (when (free-id-set-member? covered-ids expanded)
        (raise-syntax-error 'polysemy
                            "some available function cases overlap"
                            stx
                            #f
                            pred-ids))
      (free-id-set-add! covered-ids expanded)))
  ;; Move up the inheritance DAG, and see if any of the ancestors
  ;; is covered. Since we start with the parents of the user-supplied contract,
  ;; there will be no self-detection.
  (define already-recur (mutable-free-id-set))
  (define (recur pred-id)
    (unless (free-id-set-member? already-recur pred-id)
      (free-id-set-add! already-recur pred-id)
      (when (free-id-set-member? covered-ids pred-id)
        (raise-syntax-error 'polysemy
                            "some available function cases overlap"
                            stx
                            #f
                            pred-ids))
      (for-each recur (free-id-table-ref contracts-supertypes pred-id))))
  (for ([pred-id (in-list pred-ids)])
    (apply recur (free-id-table-ref contracts-supertypes
                                    pred-id))))

(define-for-syntax (the-case-dispatch-impl stx)
  (syntax-case stx ()
    [(id . args)
     (identifier? #'id)
     #`(#%app #,(the-case-dispatch-impl #'id) . args)]
    [id
     (identifier? #'id)
     (with-syntax
         ([((f-id pred-id) ...)
           (for*/list ([meaning (in-set all-meanings)]
                       [generated-name (in-value (gen-id #'id meaning))]
                       [slv (in-value
                             (syntax-local-value generated-name (λ () #f)))]
                       #:when (and slv (a-case? slv)))
             (list (a-case-f-id slv)
                   (a-case-pred-id slv)))])
       ;; Detect if there is overlap among the predicates, and raise an error
       ;; in that case.
       (detect-overlap #'id (syntax->list #'(pred-id ...)))
       ;; TODO: for now, this only supports a single argument.
       ;;       we should generalize it to support case-λ, and dispatch on
       ;;       multiple arguments
       ;; TODO: use syntax-local-lift-module-end-declaration to cache
       ;;       the generated dispatch functions.
       #`(λ (arg)
           (cond
             [(pred-id arg) (f-id arg)]
             ...)))]))

;; The only case-dispatch macro (all others are renamings of this one)
(define-syntax the-case-dispatch the-case-dispatch-impl)
