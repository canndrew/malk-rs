An alternative to using effects to control stack usage is to have some kind of
`stack_size_of` operator similar to `size_of` which calculates the stack size
of functions. The stack size may be dependent on the function argument.
Therefore `stack_size_of` should return a function.

    let f = {x: U32} => x;
    let s = stack_size_of f {23};
    
    run_with_stack: {
        'Ret: Type,
        f: {} -> Ret,
        stack: &mut Space 0x4000000,
        stack_size: ~0x4000000;
        fits: stack_size_of f #= stack_size,
    } -> Ret

