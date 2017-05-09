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
                        (only-in polysemy/private/ids
                                 [the-case-dispatch generated-normal-macro]))})
             #,@(if (identifier-binding #'generated-identifier-macro)
                    #'{}
                    #'{(local-require
                        (only-in polysemy/private/ids
                                 [the-case-dispatch
                                  generated-identifier-macro]))})
             (define/contract (tmp-f arg₀ argᵢ ...)
               (-> pred? (or/c 'argᵢ any/c) ... any)
               . body)
             (define-syntax generated-name (a-case #'tmp-f #'pred?)))))]))
