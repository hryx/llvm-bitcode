## About

This is a Zig library for reading and writing [LLVM bitcode](https://www.llvm.org/docs/BitCodeFormat.html),
a binary serialization format for [LLVM IR](https://llvm.org/docs/LangRef.html).

Status: **experimental and incomplete**.
Please see [issues](https://github.com/hryx/llvm-bitcode/issues) for remaining work and bugs.

This project has 3 components:

1. `bitstream`, a low-level interface to LLVM's abstract container and encoding format known as _bitstream_.
2. `bitcode`, which handles parsing and rendering LLVM IR _bitcode_ files, based on the bitstream format.
3. `bcanalyzer`, a command-line tool for printing information about LLVM IR bitcode files.

## Usage

Install a recent version of [Zig](https://ziglang.org/download/) and [Clang](https://releases.llvm.org/download.html).

Build and use the `bcanalyzer` CLI tool to read a bitcode file:

```
clang -emit-llvm -c my_file.c
zig build
./zig-out/bin/bcanalyzer --dump my_file.bc
```

If successfully parsed, the structure is printed as JSON, so you can pipe it to `jq`.

See `zig build --help` for more options, as usual.
