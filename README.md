Experimental reader/writer for LLVM bitcode files.

https://www.llvm.org/docs/BitCodeFormat.html

## `bcanalyzer` tool

```
clang -emit-llvm -c my_file.c
zig build
./zig-out/bin/bcanalyzer --dump my_file.bc
```
