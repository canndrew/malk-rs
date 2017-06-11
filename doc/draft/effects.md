# Effects

Do we need an effect system?

Some important effects:

## Exception

CPUs/OSes/environments suppoort unwinding so we may as well support it in the
language as well. Generally I'd prefer an enum-based approach to error
handling (ie. Either/Result), but some errors are *really*
pervasive (such as running out of memory) for these cases it may be
better to have silent exceptions. Rust's panic causes all kinds of
havoc for the type system but this could be ameliorated with a
type-system-aware form of exceptions.

Heap exhaustion is something that can happen damn near anywhere and
there's often no better way to handle it than to just give up and tear
down the program. This is the kind of things we'd rather make
invisible in the code and have the error just float to the top of the
program.

def catch: ?@{
    Ok: Type,
    Err: Type,
} -> (#{} -> Ok eff (Exception Err)) -> Result {Ok, Err};

def raise: (?Err: Type) -> (val: Err) -> #[] eff (Exception Err)

## Stack

Some effects are even more pervasive than OOM exceptions. If we want to be
serious about writing crash-proof programs then we need to protect against
stack overflows. We could do this through an effect `Stack: Um -> Effect` where
`Stack len` indicates a function uses at most `len` bytes of stack space.

This is a case where we definitely don't want to have to plumb things through
manually. Doing so would mean the user would have to explicitly
ask for and return stack space for every local variable they want to create while they explictly pass a stack pointer through every single function call.
Here's what that might look like:

    // Allocate a buffer on the stack and pass it to another function
    def call_with_buffer = (?Ret: Type)
                        => (?n: Um)
                        => (stack: Stack (1024 + n))
                        => (f: #{Ref (Array 1024 U8), Stack n} -> #{Ret, Stack n})
                        -> #{Ret, Stack (1024 + n)}
                        => (
        let @{buffer, stack} = alloca {stack, 1024};
        let @{ret, stack} = f {&buffer, stack};
        let stack = freea {stack, buffer};
    )

A nicer, effect-based version might look like this:

    // Allocate a buffer on the stack and pass it to another function
    def call_with_buffer: (?Ret: Type)
                       -> (?n: Um)
                       -> (f: Ref (Array 1024 U8) -> Ret eff (Stack n))
                       -> Ret eff (Stack (1024 + n))
                       => (
        let buffer = make_some_zeros 1024;
        f (&buffer)
    )

Even nicer would be for the compiler to infer all these stack effects for us and only complain if `main` ends up wanting more space than we can give it.

## Block

Anything that can block.

There are multiple handlers for this effect, their is the main blocking-IO handler as well as a library-supplied async handler


## UsesRegisterA

## `Spin`

may consume cpu without doing anything.

## `Loop`

We want our type system to be consistent, but at the same time proving that
every function terminates can be a real pain in the ass.
Idris handles this by making total and partial functions two different things, Only total functions can be called during type checking.

Enter: the `Loop` effect.

may contain non-terminating loops.
So that we can write code without having to prove all our functions terminate but also not end up with an inconsistent type system.


## Overflow

For signaling/capturing arithmetic overflow.

