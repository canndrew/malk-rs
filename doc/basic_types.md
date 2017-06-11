## Primitive types

### Numeric types

Malk has numeric types.

    Integers:

    Type                    Type syntax     example value syntax
    unsigned 8bit           U8              123u8
    unsigned 16bit          U16             123u16
    unsigned 32bit          U32             123u32
    unsigned 64bit          U64             123u64
    signed 8bit             I8              123i8
    signed 16bit            I16             123i16
    signed 32bit            I32             123i32
    signed 64bit            I64             123i64
    unsigned memory-sized   Um              123um
    signed memory-sized     Im              123im
    unsigned big ints       Ubig            123ubig
    signed big ints         Ibig            123ibig

These are mostly the same as Rust's numeric types except that the type names
start with an uppercase letter (eg. U32 instead of u32). This follows the
convention of type names being in UpperCamelCase although it does look kinda
ugly. Malk also has big ints as primitive types because these are very useful
for writing type-level proofs with.

### Unit type

There is a unit type, aka. C's `void`, aka. Rust/Haskell's `()`. It has three
syntactic forms:

The unit type is written `#{}`. The unit value is written `{}`. Because we're
working with dependent types it's important for these two forms to have
different syntax. In something like rust or haskell you can distinguish between
the type `()` and the value `()` based on context, but with dependent types the
type is also a value and can appear in the same positions. There is also a
pattern form (used when writing function which take a `#{}` argument) which is
also written `{}`.

### Pair type

There is a (dependent) pair type. The type form is written `#{head_name:
HeadType, .. TailType}` and the value form is written `{head_name = head_value,
.. tail_value}`. The tail type can be dependent on the head type. There's also
a pattern-match form written `{head_name = head_pattern, .. tail_pattern}`
which can be used to extract the head and the tail and bind them to variables.
The head can also be indexed by writing `my_pair.head_name`.

### Structs

The pair type would not usually be used directly in practice. Its main purpose is to
serve as a primitive for inductively building/consuming struct types. Structs contain
any number of elements and their syntax looks like this:

    type:    #{name_a: TypeA, name_b: TypeB, name_c: TypeC}
    value:    {name_a = value_a, name_b = value_b, name_c = value_c}
    pattern:  {name_a = patten_a, name_b = patten_b, name_c = patten_c}

Struct types are just syntactic sugar for tail-nested pairs terminated with a
unit. ie. The above type could also be written
    
    #{name_a: TypeA, .. #{name_b: TypeB, .. #{name_c: TypeC, .. #{}}}}

We can specify any number of head elements before collecting the rest of the
struct into the tail. So this is also valid syntax for the same type:

    #{name_a: TypeA, name_b: TypeB, .. #{name_c: TypeC}}

Malk's structs serve as a replacement for the tuple and (ordered) struct types
found in other languages. By using dependent functions (introduced later) they
can also serve as a replacement for array types.

Indexing a pair recurses into the tail element if the name of the head element
doesn't match. This way we can write `{x = 0, y = 1, z = 2}.y` to get the value
`1`.

Multiple fields of a struct can also have the same name. This is necessary
because structs are built inductively from pairs and can contain an arbitrary,
and possibly generic, type as their tail.  Although this may be a potential
foot-gun it's unlikely to matter much in practice as people will not
deliberately duplicate field names. It may occur when people are
programatically generating struct types in such a way that they can't control
the field names, but in this case they must necessarily be using
pattern-matching - not indexing - to destructure values of these types.

The tail element of a pair does not need to be a struct, this is a perfectly
valid type: `#{U32, .. U32}`. However the only way to create types and values
like this is to work directly with pairs rather than at the usual level of
structs.

Since structs are composed of pairs, the compiler is not free to arbitrarily
reorder struct elements. For a struct of *n* elements it has *2^(n - 1)*
different orderings to choose from. This is still better than C (where structs
are ordered) but not as good as (say) Rust where the compiler can reorder in
*n!* different ways.

### Function types

Malk has dependent function types. Function types are written *pattern* `->`
*return type*. Function values are written *pattern* `=>` *return value*. The
pattern specifies the argument type of the function as well as how to
destructure and name-bind the argument. The simplest pattern simply gives a
name to bind the argument to: `x => x` is the identity function on an
unspecified type. Patterns can also be parenthesised or be given type
annotations: `(x: U32) -> U32` is the type of functions which take a `U32`
(named `x`) and return a `U32`.

Struct and pair arguments can be destructured in the pattern position and names
bound to their elements:

    // function which takes a unit and returns 23u32
    {} => 23u32

    // function which takes a `#{x: U32, y: U32}` and sums the elements.
    {x = bind_x: U32, y = bind_y: U32} => bind_x + bind_y

    // another function which takes a `#{x: U32, y: U32}` and sums the
    // elements. If the names of the argument struct elements are not
    // explicitly specified then they are assumed to be the names that the
    // elements are bound to (here `x` and `y`).
    {x: U32, y: U32} => x + y

    // functions which take a pair `#{x: U32, .. U32}` and sum the elements
    {x = bind_x: U32, .. y: U32} => bind_x + y
    {x: U32, .. y: U32} => x + y

For unsigned ints, we consider them to be inductively defined by a zero and a
successor constructor and allow pattern matching on them by handling the two
cases seperately.

    // A function that takes n and returns (n - 1) (or 0 when n = 0).
    [
        0     => 0u32,
        1 + n => n,
    ];

In malk, function applictation is written without parenthesis (eg. `func arg`)
like in many functional programming languages. However many functions will take
a struct as their argument allowing us to write things like `func {123, 456}`.
Note that this also gives us named arguments for free.

There is also an alternative, back-to-front way of writing function
application: `f arg` can be written `arg in f` where `in` is a keyword.

All functions are unboxed closures. This means that a function type does not
have a fixed known size at compile time. Functions can be represented as a
function pointer followed by the closure data. The pointer can be used to
determine the size of the data (eg by storing the length as a constant before
the function's machine code). In general, all values of a given type in Malk
can have their size computed at runtime. For most types, this size can also
be known at compile time.

### Never type

The never type is a type with no values. The type is written `#[]`. There is
also a patten-match form written `[]` which is a function which maps from `#[]`
to any other type. ie. if we have `x: #[]` we can write `x in []` to get a
value of any type.

### Sum types

There are sum types (ie. the disjoint union of two types). The type form is
writting `#[left_name: LeftType, .. RightType]`. This type has two different
value constructors written `[left_name = left_value]` and `[.. right_value]`.

### Enums

Like pair types, sum types are not meant to be used directly in most
circumstances. Instead we have enum types which are just syntactic sugar for
nested sums terminated in a never. Enum syntax looks like this:

    type:       #[name_a: TypeA, name_b: TypeB, name_c: TypeC]
    values:      [name_x = value_x]

The above type is syntax sugar for the type `[name_a: TypeA, .. #[name_b:
TypeB, .. #[name_c: TypeC, .. #[]]]]`.  `[name = expr]` is ambiguous as it
could be nested right in a pair. We must use type inference and annotations to
figure out the exact type. Eg. for the above example type, a value written
`[name_b = value_b]` would be inferred to mean `[.. [name_b = value_b]]`.

### Functions out of enums

To map out of an enum type we must provide a branch for each possible variant.
The syntax is similar to that for integer types:

    [
        name_a = pattern_a => result_a
        name_b = pattern_b => result_b
        name_c = pattern_c => result_c
    ]

This can also be written, desugared, as nested matches on sums:

    [
        name_a = pattern_a => result_a,
        .. [
            name_b = pattern_b => result_b,
            .. [
                name_c = pattern_c => result_c,
                .. []
            ]
        ]
    ]

