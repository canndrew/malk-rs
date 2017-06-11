# Things that need to be figured out and organised into docs

## Mutual recursion / `let` vs `def`

if we define two things in a row, can they be mutually recursive?

    let x = y;  // ERROR - undefined `y` ??
    let y = x;

If not, it makes writing some kinds of code really annoying for no good reason.
However allowing mutual recursion can make imperitive-style
linear-type-handling code somewhat confusing:

    def @{addr, allocation, empty} = malloc {32};
    def @{full} = ptr_write {123u32, empty};
    def @{val, empty} = ptr_read {full};
    def @{} = free {allocation, empty};

We define `empty` twice here! Possibly would could infer which variable use
refers to what, but would we even want to? That could make code very confusing to read for humans.

Possible solution:
    Have both def and let. def can be mutually recursive, but a variable can
    only be bound once. let can re-bind variables but it can only refer to
    things already bound above in the code.

