#lang scribble/manual
@(require (for-label racket/base
                     polysemy))

@title{Polysemy: support for polysemic identifiers}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@defmodule[polysemy]

This is an experimental proof of concept, and is not intended to be used in
production until the potential issues of doing so have been discussed with
other racketeers.

The bindings described here may be changed in future versions without notice.

This module allows defining polysemic identifiers which can act as a
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{match expander},
as a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{macro}, as an
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}, as a
@racket[set!] subform, and as a collection of 


In all the forms below, the @racket[_meaning] should be a simple identifier.
Note that is lexical context is not taken into account (i.e. it is used as a
symbol), and therefore every @racket[_meaning] should be globally unique.
Later versions may add a notion of hygiene to meanings (allowing these
meanings themselves to be renamed, to circumvent conflicts).

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
 different meanings split out and recombined into different identifiers.}

@defform[(define-poly-case (name [arg₀ pred?] argᵢ ...) . body)]{
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
 @item[@racket[zero?]]]

 When any polysemic identifier which is contains a poly-case is called as a
 function, a check is performed to make sure that none of its cases overlap. If
 some cases overlap, then an error is raised.

 Note that an identifier cannot have both a meaning as a function case, and a
 @racket[normal-macro] or @racket[identifier-macro] meanings.}