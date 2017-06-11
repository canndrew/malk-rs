## Equality types

Malk has equality types. These are types that represent a proof that
two expressions are equal.

### Examples

This is a type:

    2 + 2 #= 4

It represents all proofs that two plus two is four. There is only one such
proof, it is the value `2 + 2 == 4` (or equivalently `4 == 4` since `2 + 2`
normalises to `4`). This value contains no data, the type as a whole is
isomorphic to `#{}`.

This is also a type:

    2 + 2 #= 5

It represents all proofs that two plus two is five. Of which there are none.
This type is empty, meaning it has no values and is isomorphic to `#[]`.

### Usage

One place you might want this is in writing a division function which requires
proof that the divisor is non-zero.

    safe_divide: {
        numerator: U32,
        divisor: U32,
        'divisor_is_non_zero: (divisor #= 0) -> #[],
    } -> U32

If we allow `divisor_is_non_zero` to be infered (see the section on inference)
then we can call this function like this:

    safe_divide {10, 2}

But if we try to call it with zero, then the inference for
`divisor_is_non_zero` will fail.

    safe_divide {10, 0}    // compile error

If inference fails (because, eg. `divisor` is not a literal) then we'll need to
manually construct a value for `divisor_is_non_zero` or pass one in from
somewhere. For example:

    divide_twice = {
        numerator: U32,
        divisor: U32,
        'divisor_is_non_zero: (divisor #= 0) -> #[],
    } => (
        let once = safe_divide {numerator, divisor, 'divisor_is_non_zero};
        let twice = safe_divide {once, divisor, 'divisor_is_non_zero};
        twice
    )

### Elimination

Equalities can be pattern-matched on. A function which takes an equality arg
can be written like:

    (a == b) => ...

Here, `a` and `b` must be the names of variables, not arbitrary expressions.
The effect of this pattern matching is that, in the function body, `a` and `b`
are treated as equal. In particular, whenever the compiler checks for the
definitional equality of two expressions it does so under a context. This
context contains all the information about the variables that are in scope and
the patterns that created them. Conceptually, the `a == b` pattern can be seen
as creating a hidden variable that both `a` and `b` normalise to - but only
during equality checking. What this means is that it can typecheck and validate
a function like this:

    let symmetry = {
        'Ty: Type,
        'a: Ty,
        'b: Ty,
        a == b,
    } => b == a;

Which is given the type:

    {'Ty: Type, 'a: Ty, 'b: Ty, a #= b} -> b #= a

We can also prove transitivity of equality similarly.

    let transitivity = {
        'Ty: Type,
        'a: Ty,
        'b: Ty,
        'c: Ty,
        a == b,
        b == c,
    } => a == c;

This typechecks because:

 0. `a == b` makes `a` and `b` both reduce to some imaginary variable `x`.
 1. `b == c` reduces to `x == c` and makes `x` and `c` both reduce to some
    imaginary variable `y`.
 2. `a == c` reduces to `y == y`, equality passes, and the body expression is
    given type `a #= c`.

Type-theory-wise, I'm pretty sure these semantics correspond to the J axiom,
but don't allow you to prove K. I would need to formalise all this to be
completely sure. Not having K is important if we want to one day extend this
language with a richer equality like in cubical type theory.

### Types are better than `Bool`.

One thing to notice here is that `1 + 1 == 2` has type `1 + 1 #= 2` - not
`Bool`. In fact, there is no equality operator that returns `Bool` because when
you have dependent types you can do much better. So what do we do if we want to
test the equality of two things? We use the `Decision` type:

    let Decision = (Ty: Type) => #[
        proof: Ty,
        disproof: Ty -> #[],
    ];

`Decision Ty` represents a proof or disproof of `Ty`, ie. either an inhabitant
of `Ty` or a proof that `Ty` is uninhabited. To test for equality between two
values `a` and `b`, we can write a function that returns a `Decision (a #= b)`.
Suppose we have such a function for `U32`, `dec_eq`, we can use it write a
wrapper for our `safe_divide` function that doesn't demand a proof of
`divisisor_is_non_zero` but returns an error on zero instead:

    let try_divide = {
        numerator: U32,
        divisor: U32,
    } -> #[ok: U32, err: divisor #= 0]
      => (
        dec_eq {divisor, 0} in [
            proof    => [error = divisor == 0],
            disproof => [ok = safe_divide {numerator, divisor, 'disproof}].
        ]
    )

This is an example of the advantage of `Decision` over `Bool`. Rather than
telling you that "something" was true/false, it tells you *what* was true/false
and carries the corresponding proof. That proof has to be used in a typesafe
way (you can't misuse a proof of `P` as a proof of `Q`) and it can be used in
typechecking (in this case by passing it to `safe_divide`).

### Cubical types?

Cubical type theory, an implementation of HoTT, provides a much richer form of
equality types where proofs of equality can carry information, many proofs can
be expressed much more simply, and where more proofs are possible to write than
in the system presented here. For example, with cubical types, you can prove
that two functions are equal when they have the same I/O behaviour (ie `f #= g`
when `f x #= g x` for all `x`) and that two types are equal when they're
isomorphic.

This is cool and useful but might clash with the idea of this being a language
for low-level programming. Fuzzing over things like the in-memory
representation of types and the computational complexity of functions makes,
for example, quicksort equal to shuffle sort. I don't know how much of a
problem this would be in practice, but I'd want to ensure that the compiler
doesn't swap out one algorithm for a less efficient one or reorder my structs
when I care about their representation. 

I need to do more investigation into how ideas from HoTT can be imported into
something like this.

