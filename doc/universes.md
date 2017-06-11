## The universe hierarchy

In malk, there is a type of types written `Type`. Using this we can write (eg.)
a generic identity function: 

    let id = { 'Ty: Type, x: Ty } => x

Strictly speaking however, `Type` is not a single type, but a function which
takes an implicit argument:

    Type : '(lev: Level) -> Type '(1 + lev)

`Level` is an int-like type whose sole purpose is to index the `Type` function.
This is necessary because just having `Type: Type` would lead to logical
inconsistancies. Instead we have a heirachy of types:

    Type '0 : Type '1 : Type '2 ...

Types like `Type 'lev` are sometimes referred to as "universes". Universes are
cummulative, in that `Type 'lev` is a subtype of `Type '(1 + lev)`.

All types themselves have a type/universe. For a type like `#{}` or `#{A, ..
B}`, its universe must be "large enough" to contain all the types that the type
contains. For instance, we have `#{}: Type '0` and, when we have both `A:
Type 'lev` and `B: Type 'lev`, `#{A, ..B}: Type 'lev`.

The `Level` type introduces its own trickiness as it cannot have any type of
the form `Type 'lev` for any `'lev` (for the sake of logical soundness).
Instead, it has type `Type+ '0`. `Type+` is a type family like `Type` except
that it's indexed by the type `Level+` and is larger (is a supertype of) all
type `Type 'lev`. `Level+` is similar to `Level` except it has type `Type++
'0`. `Type++` is a type family which is larger than `Type+` and is indexed by
`Level++`. And so forth ad infinitum.

