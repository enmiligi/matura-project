# Imp

Imp is a ML-like language made for my matura project.

## Requirements

Zig 0.15.1 is needed for both interpreter and compiler.
To switch the compiler on or off, change the variable compile in the build script build.zig.
If the compiler is used, LLVM version 20 including clang is needed as well.
On fedora 42, the packages needed are simply called 'llvm' and 'clang' each.
Install using

```
sudo dnf install llvm clang
```

## Compilation

Build the project using

```
zig build
```

The binary is then found at zig-out/bin/matura-project

## Run or Compile an Imp program

```
zig-out/bin/matura-project path-to-program
```
