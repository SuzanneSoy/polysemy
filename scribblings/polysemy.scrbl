#lang scribble/manual
@(require scribble/example
          (for-label racket/base
                     racket/contract/base
                     racket/match
                     syntax/parse
                     polysemy))

@title{Polysemy: support for polysemic identifiers}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@defmodule[polysemy]

This is an experimental proof of concept, and is not intended to be used in
production until the potential issues of doing so have been discussed with
other racketeers.

The bindings described here may be changed in future versions without notice.

@section{Examples}

This first example shows four short modules which all define the identifier
@racketid[^], with four different meanings: the first uses it as a special
token (similarly to the use of @racket[:] to separate fields from their type
in Typed Racket, among other things); the second defines it as a exclusive-or
match expander; the third defines it as the exponentiation function; the
fourth defines it as the two-variable logical xor function (which, thankfully,
does not need any short-circuiting behaviour).

@examples[#:escape UNSYNTAX
          (module m-one racket
            (require polysemy (for-syntax syntax/parse racket/list))
            (provide (poly-out [my-macro normal-macro]
                               [^ my-macro-repeat-n-times]))
            (define-poly-literal ^ my-macro-repeat-n-times hat-stxclass)
            (define-poly my-macro normal-macro
              (syntax-parser
                [(_ v :hat-stxclass n)
                 #`(list . #,(for/list ([i (in-range (syntax-e #'n))]) #'v))])))
          (module m-two racket
            (require polysemy (for-syntax syntax/parse))
            (provide (poly-out [[xor ^] match-expander]))
            (define-poly xor match-expander
              (syntax-parser
                [(_ a b) #'(and (or a b) (not (and a b)))])))
          (module m-three racket
            (require polysemy)
            (provide (all-defined-out))
            (code:comment "Multi-argument functions are not supported yet…")
            (define-poly-case (^ [x number?]) (λ (y) (expt x y))))
          (module m-four racket
            (require polysemy)
            (provide (all-defined-out))
            (define-poly-case (^ [x boolean?])
              (λ (y)
                (and (or x y) (not (and x y))))))
          (code:comment "Seamlessly require the two versions of ^")
          (require 'm-one 'm-two 'm-three 'm-four racket/match)

          (my-macro 'foo ^ 3)
          (match "abc"
            [(^ (regexp #px"a") (regexp #px"b")) "a xor b but not both"]
            [_ "a and b, or neither"])
          ((^ 2) 3)
          ((^ #t) #f)]

Thanks to the use of @racketmodname[polysemy], all four uses are compatible,
and it is possible to require the four modules without any special incantation
at the require site. The providing modules themselves have to use special
incantations, though: @racket[define-poly-literal], @racket[define-poly] and
@racket[define-poly-case]. Furthermore, a simple @racket[rename-out] does not
cut it anymore, and it is necessary to use @racket[poly-out] to rename
provided polysemic identifiers. Note that a static check is performed, to make
sure that the cases handled by @racketid[^] from @racketid[m-three] do not
overlap the cases handled by @racketid[^] from @racketid[m-four]. The function
overloads are, in this sense, safe.

The following example shows of the renaming capabilities of
@racketmodname[polysemy]: three meanings for the @racket[foo] identifier are
defined in two separate modules (two meanings in the first, one in the
second). The meanings of @racketid[foo] from the first module are split apart
into the identifiers @racketid[baz] and @racketid[quux], and the meaning from
the second module is attached to @racketid[baz]. The identifier @racketid[baz]
is therefore a chimera, built with half of the @racketid[foo] from the first
module, and the @racketid[foo] from the second module.

@examples[(module ma racket
            (require polysemy)
            (provide (all-defined-out))
            (define-poly foo match-expander (λ (stx) #'(list _ "foo" "match")))
            (define-poly-case (foo [x integer?]) (add1 x)))
          (module mb racket
            (require polysemy)
            (provide (all-defined-out))
            (define-poly-case (foo [x list?]) (length x)))

          (code:comment "baz is a hybrid of the foo match expander from ma,")
          (code:comment "and of the foo function on lists from mb.")
          (code:comment "ma's foo function is separately renamed to quux.")
          (require polysemy
                   racket/match
                   (poly-rename-in 'ma
                                   [[foo baz] match-expander]
                                   [[foo quux] (case-function integer?)])
                   (poly-rename-in 'mb
                                   [[foo baz] (case-function list?)]))

          (code:comment "baz now is a match expander and function on lists:")
          (match '(_ "foo" "match") [(baz) 'yes])
          (baz '(a b c d))

          (code:comment "The baz function does not accept integers")
          (code:comment "(the integer-function part from ma was split off)")
          (eval:error (baz 42))

          (code:comment "The quux function works on integers…")
          (quux 42)
          (code:comment "… but not on lists, and it is not a match expander")
          (eval:error (quux '(a b c d)))
          (eval:error (match '(_ "foo" "match") [(quux) 'yes] [_ 'no]))]

@section{Introduction}

This module allows defining polysemic identifiers which can act as a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{match expander},
as a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{macro}, as an
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}, as a
@racket[set!] subform, and as a collection of function overloads.

The following meanings are special:

@itemlist[
 @item{The value for the @racket[normal-macro] meaning is used when the
  identifier appears as the first element of a macro application (i.e. when it
  is used as a as a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{
   macro}).}
 @item{The value for the @racket[identifier-macro] meaning is used when the
  identifier appears on its own as an expression (i.e. when it is used as an
  @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}).}
 @item{The value for the @racket[match-expander] meaning is used when the
  identifier is used as a match template}
 @item{The value for the @racket['set!-macro] meaning is used when the
  identifier is appears as the second element of a @racket[set!] form.}
 @item{Other "core" meanings may be added later, and third-party libraries can
  define their own meanings.}]

@section{Bindings provided by @racketmodname[polysemy]}

In all the forms below, the @racket[_meaning] should be a simple identifier.
Note that is lexical context is not taken into account (i.e. the comparison is
based on the equality of symbols, not based on @racket[free-identifier=?]),
and therefore every @racket[_meaning] should be globally unique. Later
versions may add a notion of hygiene to meanings (allowing these meanings
themselves to be renamed, to circumvent conflicts).

@defform[#:kind "require transformer"
         (poly-only-in module [maybe-rename meaning ...] ...)
         #:grammar [(maybe-rename old-id
                                  [old-id new-id])]]{
 Requires each given @racket[meaning] of the corresponding @racket[old-id]. If
 @racket[new-id] is supplied, then the meanings are attached to
 @racket[new-id], otherwise they are attached to @racket[old-id].}


@defform[#:kind "require transformer"
         (poly-rename-in module [maybe-rename meaning] ...)
         #:grammar [(maybe-rename old-id
                                  [old-id new-id])]]{

 Similar to @racket[poly-only-in], but all identifiers and meanings which are
 unaffected are also implicitly required. Note that if some (but not all)
 meanings of an identifier are renamed, then the old name is not required
 automatically anymore, and needs to be explicitly required.}

@defform[#:kind "provide transformer"
         (poly-out module [maybe-rename meaning])
         #:grammar [(maybe-rename old-id
                                  [old-id new-id])]]{
 Provides the given meanings for @racket[id]. It is necessary to provide all
 the desired meanings explicitly, or use @racket[(provide (all-defined-out))].
 Simply using @racket[(provide id)] will only provide the base identifier,
 without any meanings attached to it.

 If @racket[old-id] and @racket[new-id] are supplied, each given
 @racket[meaning], which must be attached to @racket[old-id], will be
 re-attached to @racket[new-id].}

@defform*[{(define-poly id)
           (define-poly id meaning value)}]{
 The first form declares that @racket[id] is a polysemic identifier, with
 no special meaning attached to it.

 The second form attaches the phase 1 @racket[value] (i.e. it is a transformer
 value) to the given @racket[meaning] of the @racket[id].}

@defform[#:kind "pattern expander"
         (~poly pvar meaning)]{
 Pattern epander for @racketmodname[syntax/parse], can be used to match against
 polysemic identifiers, extracting the desired @racket[meaning].
 
 The transformer value for the requested meaning is stored in the
 @racket[value] attribute.}

@defform[(define-poly-literal id meaning syntax-class)]{
 Defines @racket[id] as a literal with the given @racket[meaning]. The
 @racket[syntax-class] is automatically defined to recognise the given
 @racket[meaning] of @racket[id], even if @racket[id] was renamed and its
 different meanings split out and recombined into different identifiers.

 This can be used to define "tokens" for macros, which bear a special meaning
 for some macros, but might have a different meaning for another third-party
 macro. If both rely on @racketmodname[polysemy], then they can use the same
 default name, without the risk of the identifiers conflicting. Furthermore, it
 is possible to rename the two meanings separately.}

@defform[(define-poly-case (name [arg₀ pred?]) . body)]{
 Note that the syntax for this form will be changed in the future when support
 for multiple-argument dispatch is added (remember, this package is still in an
 experimental state).

 Defines an overload for the @racket[name] function, based on the type of its
 first argument. For now, only a few contracts are allowed:

 @itemlist[
 @item[@racket[any/c]]
 @item[@racket[string?]]
 @item[@racket[exact-positive-integer?]]
 @item[@racket[exact-integer]]
 @item[@racket[integer?]]
 @item[@racket[exact?]]
 @item[@racket[number?]]
 @item[@racket[zero?]]
 @item[@racket[list?]]]

 When any polysemic identifier which is contains a poly-case is called as a
 function, a check is performed to make sure that none of its cases overlap. If
 some cases overlap, then an error is raised.

 Note that an identifier cannot have both a meaning as a function case, and a
 @racket[normal-macro] or @racket[identifier-macro] meanings.}

@defform[#:kind "poly-meaning-expander"
         (case-function pred?)]{
 When used in place of a meaning in a @racket[poly-rename-in],
 @racket[poly-only-in] or @racket[poly-out] form, expands to the meaning symbol
 for a function overload accepting the given argument type. The
 @racket[normal-macro] and @racket[identifier-macro] meanings (which would
 normally be associated with @racketmodname[polysemic]'s dynamic dispatch
 macro) are also included in the expansion.}

@defidform[#:kind "meaning"
           poly-meaning-expander]{

 When used as
 @racket[(define-poly _some-id poly-meaning-expander (λ (stx) . body))],
 defines an expander for the @racket[poly-rename-in], @racket[poly-only-in] and
 @racket[poly-out] forms. For example, the @racket[case-function] expander
 described above is defined in that way.

}

@section{Limitations}

There are currently many limitations. Here are a few:

@itemlist[
 @item{Meanings themselves cannot be renamed, and must therefore be globally
  unique. A later version could solve this by generating the actual meaning
  symbol using @racket[gensym], and by attaching it to a user-friendly name by
  means of a @racket[poly-meaning-expander].}
 @item{It should be possible to specify multiple macro cases, as long as they
  do not overlap.}
 @item{Function overloads currently only allow a single argument. Adding
  multiple dispatch and multiple non-dispatch arguments would be nice.}
 @item{Only a few contracts are supported by function overloads. For simple
  contracts, it is only a matter of extending the inheritance table in
  @filepath{ids.rkt}. More complex contract combinators will require a bit more
  work.}
 @item{The generated functions are not compatible with Typed Racket. Deriving
  types from the small set of contracts that we support should not be difficult,
  and would allow function overloads in Typed Racket (as long as the
  user-defined functions are typed, of course).}
 @item{The whole contraption relies on marshalling names. Since
  @racket[require] and @racket[provide] only care about plain names, and do not
  have a notion of scopes (which could be used to hide some of these names), I
  do not see any way to avoid this problem, while still making simple imports
  (i.e. without renaming) work seamlessly with the stock implementation of
  @racket[require].}]