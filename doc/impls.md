## Inference with impls

**Note: I've kinda gone off this whole idea, but I'm leaving this here anyway**

An `impl` allows you to give a default value for a given type. The compiler
will use this when it needs to infer the value for an otherwise unconstrained
inferred argument.

Here's a simple example:

    let MyType = #{
        x: U32,
        y: U32,
    };

    let impl {}: MyType = {x = 123, y = 456};
    
    let get_x = {?my_type: MyType} => ... ;

Here, if we call `get_x {}`, `my_type` will be infered from the value given by
the impl.

One usage of impls is that they provide a more general form of traits and
typeclasses from other languages. For example, consider the following
trait/class defined in Rust and Haskell.

    // Rust
    trait Foo {
        fn do_foo(self) -> Self;
    }

    -- Haskell
    class Foo a where
        do_foo :: a -> a

In this language, this would simply be a function of type `Type -> Type`:

    let Foo = (Ty: Type) => #{
        do_foo: Ty -> Ty,
    };

Any value of type `Foo MyType` can then be seen as an implementation of this
"trait". However, the main feature of traits/classes in languages like
Rust/Haskell is that they're unnamed - you never need to specify which
implementation you want, or construct an implementation on demand, the
implementations just sort-of exist ambiently and the compiler can find the
correct one when it needs to based on the types at hand. This is what `impl`
gives us.

Impls can also take arguments, allowing them to be defined generically over a
family of types.

    let AnotherType: (A: Type) => #{
        val: A,
    };
    
    let impl {
        A: Type,
        foo_a: Foo A,
    }: Foo (AnotherType A) = {
        do_foo = {val} => (foo_a.do_foo val),
    };
    
If the compiler finds it needs to infer a value of type `Foo (AnotherType
U32)`, it will look at the types of all the impls in scope and see if any of
them match. `Foo (AnotherType A)` matches with `A = U32` and the corresponding
value will be used if the compiler can infer a value for `foo_a`.

Impls must not conflict with each other. If an impl is defined which overlaps
with a previous impl then the two impls must be definitionally equivalent at
the region of overlap.

The value of an impl can be omitted if it can be inferred from an earlier impl:

    let impl {}: Foo = foo;
    let impl {}: Foo;       // inferred to be foo

This is useful if one needs to re-impl things into scope. For an example see
linearity.md.

