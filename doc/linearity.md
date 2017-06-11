## Linearity

### Linear types

Malk has linear types.  If a type is linear, a non-erased variable of that type
must be used exactly once. For example, if `Ty` is linear then these functions
will fail to type-check.

    // ERROR: Uses x twice
    let foo = (x: Ty) => {x, x};

    // ERROR: Doesn't use x
    let bar = (x: Ty) => {};

This function will however:

    // OKAY: x is erased, use it as many times as we like
    let qux = ('x: Ty) => {};

Linear types are useful for efficiently modelling state and side-effects while
retain pure-functional-ness. The combination of linear and dependent types in
malk is based on the one in [this
paper](https://www.google.com.au/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwjn6aG_wLXUAhVGjLwKHc4YDxsQFggrMAA&url=https%3A%2F%2Fpersonal.cis.strath.ac.uk%2Fconor.mcbride%2Fpub%2FRig.pdf&usg=AFQjCNHvGAw6QySoRfl8wQ7ThMsoJNx7-w&sig2=o5UYz60MaUFVUrnPgIzpPA)
except that we restrict ourselves to the (0, 1)-ring and, for types judged to
be non-linear, we allow them to be freely copied and destroyed as if there were
ambient copy/destroy functions for them which are always accesible.

Types are assumed to be linear unless known otherwise. Non-linear types
include: all the simple primitive types like numerics, `Type`, and equality
types as well as structs and enums whose components are all non-linear.

If we define our own type like this:

    let Point = #{
        x: U32,
        y: U32,
    };

This type will be non-linear everywhere where its definition is in scope. After
all, there's not much point in it being linear if we can easily extract its
contained `x` and `y`, copy them, and create two replicas of our original
`Point`. For this type to be non-linear we need to define it in another module,
then only import its type into scope, not its definition. For the most part,
the only types that are linear are those that are opaque or contain an opaque
type. The exception to this is linear functions...

### Linear functions

Malk actually has two kinds of functions. Non-linear functions are written
`->`/`=>` whereas linear functions are written with a double arrow `->>`/`=>>`.
Linear functions, as the name suggests, must be called exactly once. Non-linear
functions can called any number of times but cannot capture linear variables
from their environment.

