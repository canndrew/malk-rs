## Modules

Talk about metaprogramming first, introduce modules as an example of how we can do metaprogramming.

There are no modules, But there's a function which we can use at compile time to import files as expressions.

    math_stuff.malk

    def mult: @{a: U32, b: U32} => a in @[
        0     => 0,
        1 + n => b + mul {n, b},
    ];

    {
        mult = mult,
    }

    file b:

    def math_stuff = import "math_stuff.malk";

    math_stuff.mult {123, 456};

We can do this because we have dependent types. `import` has type `import: (s:
Str) -> Import s`. `Import: Str -> Type`

What if import can't find the file? We'll need exception effects or use an infered argument somewhere.

We only know mult type-wise, not definitionally. This avoids a problem we have
in idris where if we want to prove things about the behaviour of mult. If we
want to export the definitional behaviour we can do it explicitly like this:


    def mult = @{a: U32, b: U32} => a in @[
        0     => 0,
        1 + n => b + mul {n, b},
    ];
    def mult_defn = @{a: U32, b: U32}
                 -> (mult {a, b}) #= (a in @[
        0     => 0,
        1 + n => b + mul {n, b},
    ])
                 => (mult {a, b}) == (mult {a, b});

why does this mult_def thing work? what will it's actualy type be after normalisation?

research benefits of modules systems over c-style #include. What can the programmable import stuff do better?

what about impls? How are they imported if modules are just structs.

