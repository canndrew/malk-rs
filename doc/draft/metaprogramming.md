## Metaprogramming

Malk can be used as its own metaprogramming language. When we run the compiler
on a `.malk` file we are actually executing that file as a script then
converting the returned function into an executable.

A full malk program may look like this:

    // main.malk

    let io = env.use "io";

    (w: World) => (
        io.println {w, "hello"};
    )

Here, the `env` object is a value passed to the script at compile time which
can be used to (in this case) load a module from a file. `env` is linear and so
cannot be captured by the non-linear function returned by the script. As such,
it cannot be used to load modules at runtime.

