# Pointer Types

In malk, pointers always point to valid data.

We first present a memory model and then create syntactic sugar for it.

## Memory

Memory comes from two places, the stack and the heap.
For simplicity, we'll start with two assumptions which we will relax later:
  * We only have a heap. Stacks can be modelled using the heap.
  * Allocation never fails.
The heap has some types and a pair of operations for creating/destroying memory:

    def Heap: Type;
    
    def Allocation: @{
        addr: Um,
        size: Um,
    } -> Type;

    def malloc: @{
        ?heap: Heap,
        size: Um,
    } -> #{
        ?heap: Heap,
        addr: Um,
        allocation: Allocation {addr, size},
        empty: Empty {size, addr},
    };

    def free: @{
        ?heap: Heap,
        ?addr: Um,
        ?size: Um,
        allocation: Allocation {addr, size},
        empty: Empty {size, addr},
    } -> #{
        ?heap: Heap,
    };

Here, `Heap` is a linear type which is threaded through the `malloc` and `free`
operations to make them linear. `Allocation {addr, size}` is a linear type
which represents proof that there is currently an allocation of size `size` at
`addr`. The linearity of `Allocation` means that it is impossible to double-free
or leak memory as every `Allocation` must be destroyed once with `free`.

Now we need a way to represent data at these allocations. We do this using a
couple of new linear predicates:

    def Empty: @{
        size: Um,
        addr: Um,
    } -> Type;

    def Full: @{
        Ty: Type,
        addr: Um,
    } -> Type;

These represent that `addr` either contains a value of type `Ty` or is
currently unused. We then add operations for reading/writing to memory.

    def ptr_write: @{
        ?size: Um,
        ?addr: Um,
        ?Ty: Type,
        val: Ty,
        ? size_of val #= size,
        empty: Empty {size, addr}.
    } -> #{
        full: Full {Ty, addr},
    };

    def ptr_read: @{
        ?addr: Um,
        ?Ty: Type,
        full: Full {Ty, addr},
    } -> #{
        val: Ty,
        empty: Empty {size_of val, addr},
    };

Some things to note here: firstly, in malk, `size_of` is an operation of
values, not types. For some types, such as `U32` any value will have the same
size. But this is not true for other types such as `str`.
.. explain these functions more.

Example

    let @{addr, allocation, empty} = malloc {32};
    let @{full} = ptr_write {123u32, empty};
    let @{val, empty} = ptr_read {full};
    let @{} = free {allocation, empty};

So far so good. However our model so far has one major limitation: all reads
are destructive. For non-linear data we want to be able to copy-out from a
memory location without stealing the data. We can do this by defining read
operations for the various non-linear types.

    def Copy: (Ty: Type) -> Type
            = (Ty: Type) => @{
        ?addr: Um,
        full: Full {Ty, addr},
    } -> #{
        val: Ty,
        full: Full {Ty, addr},
    };

    impl @{}: Copy U32
            = read_copy_u32;
    impl @{
        Head: Type,
        Tail: Type,
        name: Str,
        Copy Head,
        Copy Tail,
    }: Copy #{Head; $name: Tail}
     = read_copy_pair {Head, Tail, name}

    def ptr_read_copy: @{
        ?addr: Um,
        ?Ty: Type,
        ? Copy Ty,
        full: Full {Ty, addr},
    } -> #{
        val: Ty,
        full: Full {Ty, addr},
    };

We want to be able to duplicate pointers.
.. So we add another arg to `Full`

    def Full: @{
        Ty: Type,
        addr: Um,
        refs: Ubig,
    } -> Type;

    def split: @{
        ?Ty: Type,
        ?addr: Um,
        ?refs: Ubig,
        full: Full {Ty, addr, refs},
    } -> #{
        full_0: Full {Ty, addr, refs + 1},
        full_1: Full {Ty, addr, refs + 1},
    };

    def join: @{
        ?Ty: Type,
        ?addr: Um,
        ?refs: Ubig,
        full_0: Full {Ty, addr, refs + 1},
        full_1: Full {Ty, addr, refs + 1},
    } -> #{
        full: Full {Ty, addr, refs},
    };

### Atomics
### Handling out-of-memory with effects

## The Syntax

    &empty 23 ~ an empty region of size 23
    &mut Ty ~ A `#{addr: Um, Erase (Full {Ty, addr, 0ubig})}`
    &Ty ~ A `#{addr: Um, Erase #{refs: Ubig, Full {Ty, addr, refs}}}`

or use this (because we can't mix up terms and types like we can in rust)

    Ref Ty
    Mut Ty
    Empty 23


&<something>
&23 ~ a compile-time constant. Give us the address into .data
&(1 + 2) ~ an expression. Allocate stack space.
&foo ~ a let variable. Allocate stack space for that variable, give us it's address.

*<something> = ...; ~ write to a variable
    might be an &empty or a &mut.
    if it's a &mut but the type is copy then we silently copy-out the data first to make an &empty.

*<something> ~ read a ptr expression.
    if it's a copy type then use ptr_read_copy, otherwise use ptr_read

Maybe the reference count should be an argument to the region type.
    
    Region: ?Ubig -> Type

a: Region ?0
so &'a foo is mutable

### Example: `Box` implementation

```
def Box: (Ty: Type) -> Type
       = (Ty: Type) => #{
    addr: Um,

};
```


### Example: `Rc` implementation





We can use this to model stack allocations as such:

    def alloca: @{
        ?heap: Heap,
        ?Res: Type,
        size: Um,
        f: (

## Let's make things abstract.

What happens when you cross machine boundaries?
Like, you compile phase on 64bit and run phase happens on 32bit? To proofs involving Um break when they get closed over by main?

    let foo = (size_of 123um) #= 64;
    let main = (w: &World) => (
        // use foo even though we're on 32bit.
    )

# Size

Every value has a size and every cell has a size which specifies which values can be put in it.
The Size type is machine-dependent: in the interpretter it's #{} because any value can be put in any call.

