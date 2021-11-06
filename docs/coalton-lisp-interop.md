# Coalton-Lisp Interoperation

## Introduction

Coalton is a language embedded in Lisp, and indeed, Coalton compiles to Lisp code. This document describes what Coalton promises for interoperation.

## Interaction Mode

First and foremost, there are two ways to globally compile Coalton. This is determined by an environment variable `COALTON_ENV` which in turns sets a global variable `*interaction-mode*`. There are two options for this variable:

- `COALTON_ENV=development`, which sets `*interaction-mode*` to `:development`
- `COALTON_ENV=release`, which sets `*interaction-mode*` to `:release`

As the names imply, these two modes have different behaviors that optimize for either development or release.

**WARNING**: *All* Coalton code, *including* the standard library, *must* be compiled in the same mode. We advise application writers to set the environment variable during their application's build process before Coalton is compiled, and to ensure caches are not stale.

By default, the interaction mode is `:development`.

### Development Mode

Development mode allows most structures to be re-defined. This means:

* Most types turn into CLOS classes.
* Most types are not flattened or unwrapped in any way.
* Certain optimizations that obscure debugging are disabled.

### Release Mode

Release mode freezes most structures and optimizes them.

* Most types are defined as frozen `defstruct`s or flattened Lisp data types.
* Many optimizations are performed.

### Pitfalls of Having Two Modes

Unfortunately, having two modes may mean one inadvertently depend on the behavior in one mode that is not supported in the other. We advise testing your code against both modes.

## Promises of Data and Basic Data Types

**PROMISE**: Every Coalton value exists as a Lisp value. The structure of said value may not always be defined, however, and must be considered an element of type `T` in general circumstances.

**PROMISE**: Coalton `Integer`, `Character`, `String`,  `Single-Float`, and `Double-Float` values correspond to their Lisp counterparts.

**PROMISE**: `COALTON:Boolean` is a Lisp `BOOLEAN`; `COALTON:True` is Lisp `T`, and `COALTON:False` is Lisp `NIL`.

**WARNING**: Lists and other data types may not correspond to Lisp's conception of them.

## Promises of  `define-type`

For the most part, `define-type` will not provide guarantees across interaction modes. One reason to understand why is to consider the type

```
(define-type Wrapper
  (Wrap Integer))
```

Depending on the interaction mode, `Wrap` may actually be similar to the `IDENTITY` function, and `Wrapper` may be representation-equivalent to `Integer`.

Nonetheless, there is one practical guarantee: the existence of Lisp functions for unconstrained constructors. *Unconstrained* means that there are no constraints on any type variables present in the type that describes the value. Consider the following type:

```
(define-type Foo
  (Ctor1 ...)
  Ctor2
  ...)
```

**PROMISE**: For each unconstrained constructor function (e.g., `Ctor1`), there will be a corresponding Lisp function function-bound to the constructor's name. This function will have the same arity as specified.

**PROMISE**: For each unconstrained nullary constructor (e.g., `Ctor2`), there will be a corresponding Lisp value bound to the constructor's name.

**WARNING**: Coalton gives no guarantee about the structure of the values that are constructed by the constructors in Lisp.

### Achieving Guaranteed Lisp Compatibility with `(REPR :LISP)`

The pragma `(repr :lisp)` helps achieve Lisp compatibility of structures regardless of interaction mode, at the expense of optimization opportunities. Consider a type:

```
(repr :lisp)
(define-type (Foo ...)
  (Ctor1 ...)
  Ctor2
  ...)
```

**PROMISE**: `FOO`, `FOO/CTOR1`, `FOO/CTOR2`, etc. will be valid symbols denoting disjoint class names checkable by `typep` and dispatchable by `defmethod`.

**WARNING**: It is undefined whether these classes are `standard-class` or `structure-class` or something else.

**PROMISE**: `(subtypep 'FOO/* 'FOO)` will be true for valid substitutions of `*`.

**PROMISE**: Even if there is only a single constructor, a separate disjoint type will be created.

**PROMISE**: Non-nullary, unconstrained constructors (e.g., `CTOR1`) will be Lisp functions that return the type as specified in the previous promise.

**PROMISE**: Nullary, unconstrained constructors (e.g., `CTOR2`) will be Lisp values that are of the return type specified in the previous promise.

**Todo?**: Should we promise generated accessors?

## Promises of `define`

Consider the following definitions:

```
(define v       x1)
(define (f ...) x2)
```

**PROMISE**: Assuming they are unconstrained, `v` and `f` will be lexical variables in Lisp bound to those symbols.

**PROMISE**: Assuming it is unconstrained, `f` will be a function in Lisp of the same arity found at the definition site.

## Lisp-Calls-Coalton Bridge

**Todo**: `COALTON` operator

## Coalton-Calls-Lisp Bridge

**Todo**: `LISP` operator

## Coalton's `Lisp-Object` Type

For low-level code, such as in the standard library, it is sometimes necessary to stash a Lisp object in a Coalton data type. This is done by specifying a field in an algebraic data type to be `Lisp-Object`.

**PROMISE**: `Lisp-Object`s are not wrapped in their runtime representation in any way.

**PROMISE**: For a suitable, unconstrained constructor function that takes a `Lisp-Object`, one may safely call that constructor directly on a Lisp object of interest.

## Type Correctness/Soundness in Interop

**Todo**