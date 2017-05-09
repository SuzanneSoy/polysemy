#lang racket/base

(provide
 ;; A require transformer
 poly-rename-in
 ;; Another require transformer
 poly-only-in
 ;; Provide transformer
 poly-out
 ;; Definition of a polysemic id, and of a part of a polysemic id
 define-poly
 ;; Syntax-parse pattern expander which extracts the given meaning from the id
 (for-syntax ~poly)
 ;; Defines a literal which can be renamed, without conflicting with other
 ;; poly literals, or identifiers with other meanings.
 define-poly-literal
 ;; TODO: move this to ids.rkt
 the-case-dispatch
 ;; Defines a static overload for a polysemic method
 define-poly-case)

(require "private/ids.rkt"
         racket/contract ;; TODO: remove if not needed.
         (for-syntax racket/base
                     racket/list
                     racket/set
                     racket/require-transform
                     racket/provide-transform
                     syntax/parse
                     syntax/id-table
                     syntax/id-set
                     "private/utils.rkt"
                     racket/contract
                     racket/syntax)
         (for-meta 2 racket/base))

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

;; Require transformers
;; _____________________________________________________________________________

;; Common implementation for the poly-rename-in and poly-only-in rename
;; transformers.
(define-for-syntax (poly-require-transformer req stx)
  (syntax-parse stx
    [(_ mod
        [{~or {~and :id old-id new-id}
              (old-id:id new-id:id)}
         meaning:id
         ...]
        ...)
     #:with ((old-generated-id ...) ...)
     (map (λ (id meanings) (map (λ (meaning) (gen-id id (syntax-e meaning)))
                                (syntax->list meanings)))
          (syntax->list #'(old-id ...))
          (syntax->list #'((meaning ...) ...)))
     #:with ((new-generated-id ...) ...)
     (map (λ (id meanings) (map (λ (meaning) (gen-id id (syntax-e meaning)))
                                (syntax->list meanings)))
          (syntax->list #'(new-id ...))
          (syntax->list #'((meaning ...) ...)))
     #:with (new-id-no-duplicates ...)
     (remove-duplicates (syntax->list #'(new-id ...))
                        free-identifier=?)
     #:with (new-safeguard-no-duplicates ...)
     (map (λ (one-id) (gen-id one-id '| safeguard |))
          (syntax->list #'(new-id-no-duplicates ...)))
     (register-meanings (syntax->datum #'(meaning ... ...)))
     (expand-import
      #`(combine-in
         ;; We always require the same ids, so that multiple requires
         ;; are a no-op, instead of causing conflicts.
         (only-in polysemy/private/ids
                  [the-polysemic-id new-id-no-duplicates] ...
                  [the-safeguard-id new-safeguard-no-duplicates] ...)
         (#,req mod [old-generated-id new-generated-id] ... ...)))]))

;; Require transformer which allows renaming parts of polysemic identifiers.
(define-syntax poly-rename-in
  (make-require-transformer
   (λ (stx) (poly-require-transformer #'rename-in stx))))

;; Require transformer which allows selecting and renaming parts of polysemic
;; identifiers.
(define-syntax poly-only-in
  (make-require-transformer
   (λ (stx) (poly-require-transformer #'only-in stx))))

;; Provide transformers
;; _____________________________________________________________________________

(define-syntax poly-out
  (make-provide-pre-transformer
   (λ (provide-spec modes)
     (syntax-parse provide-spec
       [(_ [{~or {~and :id old-id new-id} (old-id:id new-id:id)} meaning ...]
           ...)
        (with-syntax ([((old-generated-id ...) ...)
                       (map (λ (one-id meanings)
                              (map (λ (one-meaning)
                                     (gen-id one-id (syntax-e one-meaning)))
                                   (syntax->list meanings)))
                            (syntax->list #'(old-id ...))
                            (syntax->list #'((meaning ...) ...)))]
                      [((new-generated-id ...) ...)
                       (map (λ (one-id meanings)
                              (map (λ (one-meaning)
                                     (gen-id one-id (syntax-e one-meaning)))
                                   (syntax->list meanings)))
                            (syntax->list #'(new-id ...))
                            (syntax->list #'((meaning ...) ...)))]
                      [(safeguard ...)
                       (map (λ (one-id) (gen-id one-id '| safeguard |))
                            (syntax->list #'(new-id ...)))])
          (register-meanings (syntax->datum #'(meaning ... ...)))
          (pre-expand-export #'(combine-out new-id ...
                                            safeguard ...
                                            (rename-out [old-generated-id
                                                         new-generated-id]
                                                        ... ...))
                             modes))]))))

;; Definition of polysemic identifiers and parts of these
;; _____________________________________________________________________________

(define-syntax (define-poly stx)
  (syntax-case stx ()
    ;; Definition of a new polysemic identifier
    [(_ id)
     (with-syntax ([safeguard (gen-id #'id '| safeguard |)])
       ;; TODO: this won't handle local shadowings very well.
       (if (and (identifier-binding #'id) (identifier-binding #'safeguard))
           #'(begin)
           #`(local-require
              (only-in polysemy/private/ids
                       #,@(if (identifier-binding #'id)
                              #'{}
                              #'{[the-polysemic-id id]})
                       #,@(if (identifier-binding #'safeguard)
                              #'{}
                              #'{[the-safeguard-id safeguard]})))))]
    ;; Definition of a part of a (possibly new) polysemic identifier
    [(_ id meaning value)
     (with-syntax ([safeguard (gen-id #'id '| safeguard |)]
                   [generated-id (gen-id #'id (syntax-e #'meaning))])
       (with-syntax ([define-meaning #'(define-syntax generated-id value)])
         (register-meanings (syntax->datum #'(meaning)))
         ;; TODO: this won't handle local shadowings very well.
         (if (and (identifier-binding #'id) (identifier-binding #'safeguard))
             #'define-meaning
             #'(begin
                 (define-poly id)
                 define-meaning))))]))

;; Syntax-parse pattern expander which extracts the given meaning from the
;; matched id
(begin-for-syntax
  (define-syntax-class (poly-stxclass meaning)
    #:attributes (value)
    (pattern pvar:id
             #:attr value (syntax-local-value (gen-id #'pvar meaning)
                                              (λ () #f))
             #:when (attribute value)))
  (define-syntax ~poly 
    (pattern-expander
     (λ (stx)
       (syntax-case stx ()
         [(_ pvar meaning)
          ;; Do we need to (register-meanings #'(meaning)) here? I think not.
          #'{~and {~var pvar (poly-stxclass 'meaning)}}])))))

(define-syntax-rule (define-poly-literal initial-id meaning syntax-class)
  (begin
    (define-poly initial-id meaning
      (λ (stx) (raise-syntax-error 'initial-id "reserved identifier" stx)))
    (begin-for-syntax
      (define-syntax-class syntax-class
        #:attributes ()
        ;; TODO: the description is not present in error messages. Why ?
        ;#:description
        ;(format "the ~a meaning (originally bound to the ~a identifier)"
        ;        'meaning
        ;        'initial-id)
        (pattern {~poly _ meaning})))))

(begin-for-syntax
  (struct a-case (f-id pred-id) #:transparent))

;; TODO: multimethods
(define-syntax (define-poly-case stx)
  (syntax-case stx ()
    [(_ (name [arg₀ pred?] argᵢ ...) . body)
     (let ([meaning (string->symbol
                     (format "~a" `(poly-case ,(syntax-e #'pred?))))])
       (with-syntax
           ([generated-name (gen-id #'name meaning)]
            [generated-normal-macro (gen-id #'name 'normal-macro)]
            [generated-identifier-macro (gen-id #'name 'identifier-macro)])
         (register-meanings `(,meaning))
         #`(begin
             (define-poly name)
             ;; TODO: provide keywords to selectively disable the
             ;; identifier-macro or normal-macro behaviours. Also check that
             ;; if identifier-binding does not return #f, it returns a binding
             ;; for the-case-dispatch, and not for something else.
             #,@(if (identifier-binding #'generated-normal-macro)
                    #'{}
                    #'{(local-require
                        (only-in polysemy
                                 [the-case-dispatch generated-normal-macro]))})
             #,@(if (identifier-binding #'generated-identifier-macro)
                    #'{}
                    #'{(local-require
                        (only-in polysemy
                                 [the-case-dispatch generated-identifier-macro]))})
             (define/contract (tmp-f arg₀ argᵢ ...)
               (-> pred? (or/c 'argᵢ any/c) ... any)
               . body)
             (define-syntax generated-name (a-case #'tmp-f #'pred?)))))]))

(define-for-syntax contracts-supertypes #f)
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
             (,#'zero? . ,#'integer?)
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
     (let ()
       (define/with-syntax ((f-id pred-id) ...)
         (for*/list ([meaning (in-set all-meanings)]
                     [generated-name (in-value (gen-id #'id meaning))]
                     [slv (in-value
                           (syntax-local-value generated-name (λ () #f)))]
                     #:when (and slv (a-case? slv)))
           (list (a-case-f-id slv)
                 (a-case-pred-id slv))))
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

(define-syntax the-case-dispatch the-case-dispatch-impl)
