## Implicit and erased arguments

_Note: I'm not sure that actually makes sense to identify erased and implicit
variables like I propose here. I'm trying to cut down on syntax, and
inferableness and irrelevance often (but not always) coincide._

Some values have no runtime relevance and only need to exist for typechecking.
For this we have erased arguments.

### Implicit function args.

There is a second kind of function that allows the function argument to be
implicit. Here's an example of a function type with an implicit argument marked
with `'`.

    f: '(n: U32) -> (v: Foo n) -> U32

To call this function, we don't need to give the value of `n` explicitly if it
can be inferred. For example:

    let foo: Foo 23 = ... ;
    f foo

Here, the compiler matches `Foo 23` against `Foo n` and infers `n` to be 23.
Implicit args can also be passed explicitly if need be. The syntax in this case
would be:

    f '23 foo

Implicit arguments are erased - their data does not exist at run-time. Erased
values can only by used inside erased expressions such as those used in type
positions and as implicit arguments to functions:

### Implicit struct members.

The head of a pair can also be implicit. For example there would be a type

    #{'n: U32, foo: Foo n}

These follow the same rules as function arguments: they're erased and can be
given explicitly using the `'` syntax:

    let foo: Foo 23 = ... ;
    
    {foo = foo}             // n is inferred
    {'n = 23, foo = foo}    // n is given explicitly

### Implicit let variables.

`let` expression (which are just a sugared form of function application at the
lambda-calculus level) can also be used to create erased variables:

    let 'n = 23;

These variables can use erased data but only exist for type-checking.

