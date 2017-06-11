# Codegen

Currently, malk is executed purely in a (very slow) interpretter. Options for
proper code generation:

  * LLVM
  * libfirm
  * Rust MIR
  * WebAssembly
    This is probanbly the best option - it's simple, and there's a lot of work
    being done on it to make it fast, provide tooling, etc.

