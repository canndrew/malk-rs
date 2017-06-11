# Ideas for controlling layout

Reasons for allowing control of layout:
  * So that things like C-structs and packed structs can be library-defined.
  * So that we can remove data from the middle of a struct/enum and still have
    a valid term (with a wierd layout). eg.

    let x: #[a: A, b: B, c: C] = ...;
    x in [
        b => ...,
        .. rest => // rest is valid here. has type #[a: A, <missing variant> .. #[c: C]]
    ]

  * A sensible way to have bit-fields and sub-byte sized types.

## Possible syntax for pairs:

    #{a: A, >> x .. B}

A pair with `x` bits of padding after the head, layed-out left to right.

    #{a: A, << x .. B}

A pair with `x` bits of padding after the head, layed-out in reverse.

## Possible syntax for eithers:

    #[a: A, >> x .. B]

An either with `x` missing variants in-between B and A.

    #[a: A, << x .. B]

An either where `A`'s tag starts again at `x`.
Although this syntax looks like the pair syntax, it really means something completely different. Is this good or bad?

