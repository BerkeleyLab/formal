```ascii
___________                         .__   
\_   _____/__________  _____ _____  |  |   
 |    __)/  _ \_  __ \/     \\__  \ |  |   
 |     \(  <_> )  | \/  Y Y  \/ __ \|  |__ 
 \___  / \____/|__|  |__|_|  (____  /____/
     \/                    \/     \/          
```

Formal: Fortran mimetic abstraction language
============================================

_Towards a formally verifiable, tensor-calculus langauge embedded in Fortran 2028._

## Examples
See this repository's [example](./example) subdirectory for demonstrations of how
to use Formal.  For usage information for each example, execute something like
```bash
fpm run --example <base-name> -- --help
```
where `base-name` is the portion of an example file name preceding the `.F90` or
`.f90` extension.  To save typing in a terminal window, set the `example`
directory as your present working directory before issuing the above `fpm run`
command.  Then use tab completion to enter enter the base name and delete the
file extension before pressing `return`.

## Prereqisite

Building and testing Formal requires the Fortran Package Manager  ([`fpm`]),
which can be obtained via package manager (e.g., `brew install fpm` on macOS)
or by compiling the single-file concatenation of the `fpm` source that is
included among the release assets.  For the `fpm` 0.12.0 release, for example,
compiling [fpm-0.12.0.F90] and placing the resulting executable file in your
`PATH` suffices.

## Building and testing

| Vendor | Compiler    | Version(s) | Build/Test Command                                    |
|--------|-------------|------------|-------------------------------------------------------|
| GCC    | `gfortran`  | 14-15      | fpm test --compiler gfortran --profile release        |
| GCC    | `gfortran`  | 13         | fpm test --compiler gfortran --profile release --flag "-ffree-line-length-none"|
| Intel  | `ifx`       | 2025.1.2   | FOR_COARRAY_NUM_IMAGES=1 fpm test --compiler ifx --flag "-fpp -O3 -coarray" --profile release |
| LLVM   | `flang-new` | 20-21      | fpm test --compiler flang-new --flag "-O3"            |
| LLVM   | `flang-new` | 19         | fpm test --compiler flang-new --flag "-O3 -mmlir -allow-assumed-rank" |
| NAG    | `nagfor`    | 7.2        | fpm test --compiler nagfor --flag "-O3 -fpp"          |

**Known Issues**
1. With `fpm` versions _after_ 0.12.0, `flang-new` can be shortened to `flang`.
2. With NAG 7.2, Build 7235 or later is recommmend, but earlier builds might work.

## Documentation
The [`doc/uml/class-diagram.md`] file contains a Mermaid script that generates a
Unified Modeling Language (UML) class diagram depicting many Formal derived
types and their interrelationships.  GitHub's web servers render the diagram
graphically when viewed in a web browser.

[`fpm`]: https://github.com/fortran-lang/fpm
[fpm-0.12.0.F90]: https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0.F90
[`doc/uml/class-diagram.md`]: ./doc/uml/class-diagram.md
