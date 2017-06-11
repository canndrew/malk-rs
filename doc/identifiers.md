## Identifiers and the `$`-syntax

For the sake of metaprogamming, the name of any variable can be given using an
expression of type `Str` and a dollar sign. Like this:

    // All these are equivalent

    let wow = xxx ;

    let $"wow" = xxx ;

    let foo = "wow"; let $foo = xxx ;

This can be used both for binding names and using variables - ie. the following
is also allowed.

    let foo = 23;
    let bar = "foo";

    // all of these print 23
    print foo
    print $"foo"
    print $bar

Here's an example of how you could use this feature to create struct types from
templates.

    let rec mk_struct = {'n: Um, .. elems: Vec {n, #{name: Str, Ty: Type}}}
                     -> Type
                     => (
        n in [
            0       => #{},
            1 + m   => elems in {first, .. rest} => #{
                $first.name: first.Ty,
                .. mk_struct rest
            },
        ]
    );

    mk_struct {} == #{}
    mk_struct {{"a", U32}} == #{a: U32};
    mk_struct {{"a", U32}, {"b", U64}} == #{a: U32, b: U64};

Note that this feature does not have to interfere with static typing. In the
compiler, all variables can be named with an expression of type `Str`. These
expressions do not have to be string literals (though they usually will be),
they can be any Str-typed expression including ones that contains references to
other variables. When the compiler looks up a variable in scope it finds the
variable whose name is the exact expression in question. No dynamic lookup
happens at runtime. This means, for example, variables with variable names
cannot unexpectedly shadow other variables. eg. this function will always print
123, even if called with `bar = "foo"`:

    print_123 = {bar: Str} => (
        let foo = 123;
        let $bar = 456;
        print foo
    );

