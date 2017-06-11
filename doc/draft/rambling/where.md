## Where clauses

pattern-match branching syntax defined so far doesn't allow grouping of patterns.
    what's a better way to say this?

however even rust/haskell are pretty limited. we often end up needing closures if we want more complex control flow without repeating ourselves.

the where statement, like in many functional languages, appears after an expression and allows binding a name used in that expression. like this

    (x + 1) where x = 3;  // this equals 4

it is only computed when it's needed.
using this we can construct arbitrary non-cyclic control flow graphs.

[
    red   => a,
    green => a,
    blue  => "blue",
] where a = "not blue";

where's can implicitly take arguments:
    
[
    {n, red}        => a,
    {1 + n, green}  => a,
    {_, blue}       => "blue",
] where a = "not blue, also {}" % n

Here, `n` is used in the where clause even though it's not in-scope where the
where is used. However it is in-scope both places where `a` is used.

