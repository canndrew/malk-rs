## Recursion

Recursion in dependently typed languages can be tricky to do right. You can't
just allow any function to call any other function with any argument at any
time or else the language will be inconsistent as a logic and none of the
typechecking will be valid. A trivial example of this is that you could write a
function that returns `#[]` by just calling itself in a loop: `let f = {} ->
#[] => (foo {})` and then you can create a term `foo {}: #[]`. Seen as a logic,
this corresponds to allowing proofs that contain circular logic to prove a
negative (ie. `#[]` is true because `#[]` is true).

To fix this we have to guarantee that all recursion is *well-founded*. This
means that if a function somehow ends up calling itself, it can only do so if
the sub-invocation is given an argument that is "smaller" than the argument
given to the original invocation. Here, the meaning of "smaller" can depend on
the type of the argument in question, we just need to guarantee that if you
take any argument (eg. `n: U32`) and keep making it smaller and smaller (eg. `n
=> n - 1`) you eventually won't be able to make it any smaller (eg. because `n
== 0`) and therefore recursion has to stop.

Languages like Idris and Agda achieve this through an analysis pass which
analyses the call-graph of all the functions in your code looking for cycles.
If it finds one (eg. `f` calls `g` calls `h` calls `f`) then it makes sure that
the argument given to the sub-invocation is smaller than the original. This
approach has a couple of problems though. Firstly, it's not clearly visible in
the code whether a function is valid or not because you need to consider all
the functions that it may call indirectly. Secondly, it uses a very strict
definition of "smaller than" which isn't reflected in the language and doesn't
allow the programmer to construct their own proofs that a recursive call is
valid. (Edit: Agda is now adding something like this which I should investigate...).

Malk solves this through the addition of two new kinds of type, recursive types
and subterm types, which we'll explore below.

Although restricting recursion in the way described below makes the language
non-turing-complete, this isn't as bad as it might sound. Obstensibly, it just
means it's not possible to write a program that goes into an infinite loop
without ever producing output (which is generally a thing), but we also have the option of adding
turing-completeness back in through an intrinsic or effect only available at
runtime,

### `Rec` types

In malk we can only use a variable once it's been assigned a value:

    let x = y;  // ERROR! y is undefined
    let y = 2;

We can also shadow previous variables by rebinding the name:

    let x = 1;
    let x = x + 1;
    x == 2

Given this, how could we even *try* to define a recursive type?

Enter:`Rec`. `Rec` allows you to write expressions of type `Type` which are
recursive.

    Rec Nat #[zero: #{}, pred: Nat]

The above expression represents the type of unary natural numbers. The first
argument to `Rec` is a name which will refer to the name of the type we are
defining. The second argument is a type expression which may refer to the given
name (in this case `Nat`). The expression given above refers to an infinite
type which looks like:

    #[zero: #{}, pred: #[zero: #{}, pred: #[zero: #{}, pred: ... ]]]

If we want to assign this type to a variable called `Nat` we can do so in two
ways: either the long way or using the shorthand `let Rec` syntax sugar:

    let Nat: Type = Rec Nat #[zero: #{}, pred: Nat];
    // alternatively
    let Rec Nat = #[zero: #{}, pred: Nat];

Once we've defined our type we can create terms and match on them normally like this:

    let zero: Nat = [zero = {}];
    let one: Nat = [pred = [zero = {}]];

    let is_zero: Nat -> Bool
               = [
        zero => [true = {}],
        pred => [false = {}],
    ];

However if we want to write a recursive function out of `Nat` we have to do
things a little differently.

    let is_even: Nat -> Bool
               = rec is_even (n: Nat) => n in [
        zero => [true = {}],
        pred => not (is_even_n pred),
    ];

Adding `rec is_even` before the function makes this a recursive function where
`is_even` is the name of the function we're defining, bound within the function
body. However there's a twist: within the body of the function, `is_even` has
type `~n -> Bool`, even though the function has a whole has type `Nat -> Bool`.
Here, `~n` is a subterm type, described below.

By the way, there is a syntax sugar for writing recursive function such as `is_even`:

    let rec is_even = (n: Nat) => n in [
        zero => [true = {}],
        pred => not (is_even_n pred),
    ];

### Subterm types `~expr`

For any expression `expr` there is a type of subterms of that expression,
`~expr`, representing all terms with the same type as `expr` that are
*structurally smaller* than `expr`. Here, the meaning of "structurally smaller"
depends on the type of `expr`:
  * For unsigned ints, "structurally smaller" means "less than".
  * For strings it means "substring of".
  * For `[head = super]` it means terms `[head = sub]` where `sub` is
    structurally smaller than `super`.
  * For `[.. super]` it means either `[head = anything]` or `[.. sub]` where sub
    is structurally smaller than `super`.
  * For `{super_a, super_b}` it means `{sub_a, sub_b}` where `sub_a` is
    structurally smaller than `super_a` OR `sub_a` equals `super_a` and `sub_b`
    is structurally smaller than `super_b`.
  * For recursive types, it means the subterm is equal to or structurally
    smaller than one of the occurances of the type within itself in the super
    term. eg. for `Nat`, `foo` is smaller than `[pred = foo]` and `[pred =
    [pred = foo]]` etc.
  * For every other type, no term is structurally smaller than any other.

It should be clear that the above rules define a well-founded partial ordering
for every type.

Now that we have some definitions, let's look at that `even` function up close
and see that it typechecks.

    let is_even: Nat -> Bool
            = rec is_even (n: Nat) => (

        // Here we have:
        //    is_even_n: ~n -> Bool
        //    n: Nat

        n in [
            zero => [true = {}],
            pred => (
                
                // Here, we know that pred was extracted from n as a recursive
                // occurance of Nat within itself. pred can therefore be
                // typechecked as a ~n.
            
                not (is_even_n pred)
            ),
        ]
    );

### Subtyping on subterm types.

For any `expr: E`, `~expr` is a subtype of `E`. This allows us to prove
theorems about subterms which may be useful in situations where we have more
complicated recursion. For example, earlier we said.

> For `{super_a, super_b}` it means `{sub_a, sub_b}` where [..] `sub_a` equals
> `super_a` and `sub_b` is structurally smaller than `super_b`.

We can prove this as such:

    let wow = {a, sub_b}: #{A, ~super_b}
           => {a, sub_b}: ~{a, super_b}

When typechecking this function, the typechecking algorithm will
  * Check whether `a` has type `~a`
  * It doesn't (because `a` has type `A`). So instead check whether `a` is
    judgementally equal to `a`.
  * It is, so check whether `sub_b` has type `~super_b`.
  * It does, so `{a, sub_b}` can have type `~{a, super_b}`.

This rather limited form of subtyping does not carry with it some of the
problems found with richer forms of subtyping. There is no need to translate
between the different types. For example, with `expr: E`  a pointer to type
`~expr` is also a pointer to type `E` even at the lowest level of the language
implementation.

