# Lisp Compiler (in Rust)

This is a simple compiler for a small, LISPy language, written in Rust.

## Goals

This is a personal project. I have three main goals:

- Write a working compiler, targeting 64-bit x86 (on either Windows or Linux), that can compile "Hello, world!" and a few other trivial programs.
- Write a compiler using continuance-passing style, without using a stack. For this, I'm using A-Normal form, with eager argument evaluation followed by a GOTO jump to each function body.
- Practice writing a non-trivial program in Rust.
- Practice working with assembly / low-level intermediate languages, such as LLVM.

## Implementation choices

I previously started [a similar compiler](https://github.com/paxromana96/LispCompiler) in C, but found it too frustrating and difficult to test. I've found that the Rust language and standard library are much better for implementing compilers! Further, the Rust ownership system helps me keep this program memory-safe and tight on usage.

Everyone I've shown this project has asked why I used Rust instead of Go. I like the type/trait system better than Go (especially generics!), and I REALLY appreciate `rustc`'s suggestions for correcting semantically-invalid code. Furthermore, I've written serious programs in Go before but not in Rust; I wanted the chance to practice writing a nontrivial Rust program.

## Contributing

I am not accepting contributions at this time.

## License

This code is licensed under the [MIT License](https://opensource.org/licenses/MIT).