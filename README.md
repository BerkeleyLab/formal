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

_Towards an embedded domain-specific language (DSL) for tensor calculus and formal verification._

Introduction
------------
Formal supports research on mimetic software abstractions for tensor calculus by providing

- Derived types that mimic tensor fields and
- Differential and integral operators for writing tensor expressions.

Formal's types and operators implement the discrete calculus of [Castillo & Corbino (2020)]:
mimetic discretizations satisfying discrete versions of tensor calculus theorems.

Like the underlying numerical methods, Formal's software abstractios mimic their tensor calculus counterparts.
For example, given scalar and vector fields $f$ and $\vec{v}$ defined over a unit volume $V = [0,1]^3$ bounded
by a surface area $A$, the program [`example/extended-gauss-divergence.F90`] demonstrates satisfaction of the
extended Gauss divergence theorem: 

$$ \iiint_V \vec{u} \cdot \nabla f dV + \iiint_V f \nabla \cdot \vec{v} dV = \iint_A f \vec{v} \cdot d\vec{A} $$

Running the program as follows
```fortran
fpm run --example extended-gauss-divergence --compiler flang-new --flag -O3
```
produces output that includes actual program syntax:
```fortran
    f = (x**2)/2 ! <-- scalar function
    v = x        ! <-- vector function
.SSS. (v .dot. .grad. f) * dV =  .3333333330205934
.SSS. (     f * .div. v) * dV =  .16666666739857125
   -.SS. (f .x. (v .dot. dA)) = -.5000000004191649
----------------------------------------------------
                          sum = -.2220446049250313E-15 (residual)
```
where the small residual of approximately $-.222 \times 10^{-15}$ evidences a highly accurate approximation.

**Future work:** Formal lays a foundation for defining a DSL embedded in Fortran via template requirements,
a feature of the forthcoming standard known informally as Fortran 202Y.

Examples
--------
See this repository's [example](./example) subdirectory for demonstrations of how
to use Formal.  For usage information for each example, execute something like
```bash
fpm run --example <base-name> -- --help
```
replacing `<base-name>` with the portion of an example file name preceding the `.F90` or
`.f90` extension.  To save typing in a terminal window, set the `example` directory as
your present working directory before typing `fpm run` above.  Then use tab completion to
enter a file name and delete the file extension before pressing `return` or `enter`.

Prerequisite
------------
Building and testing Formal requires the Fortran Package Manager  ([`fpm`]),
which can be obtained via a package manager (e.g., `brew install fpm` on macOS)
or by compiling the single-file concatenation of the `fpm` source that is
included among the release assets.  For the `fpm` 0.12.0 release, for example,
compiling [fpm-0.12.0.F90] and placing the resulting executable file in your
`PATH` suffices.

Building and testing
--------------------
 Vendor | Compiler    | Version(s) Tested | Build/Test Command
--------|-------------|-------------------|-------------------
 GCC    | `gfortran`  | 14-15             | fpm test --compiler gfortran --profile release
 GCC    | `gfortran`  | 13                | fpm test --compiler gfortran --profile release --flag "-ffree-line-length-none"
 Intel  | `ifx`       | 2025.1.2          | FOR_COARRAY_NUM_IMAGES=1 fpm test --compiler ifx --flag "-fpp -O3 -coarray" --profile release
 LLVM   | `flang-new` | 20-21             | fpm test --compiler flang-new --flag "-O3"
 LLVM   | `flang-new` | 19                | fpm test --compiler flang-new --flag "-O3 -mmlir -allow-assumed-rank"
 NAG    | `nagfor`    | 7.2 Build 7242    | fpm test --compiler nagfor --flag "-O3 -fpp"

With `fpm` versions _after_ 0.12.0, `flang-new` can be shortened to `flang` in the above `fpm` commands.

Documentation
-------------
The [`doc/uml/class-diagram.md`] file contains a Mermaid script that generates a
Unified Modeling Language (UML) class diagram depicting many Formal derived
types and their interrelationships.  GitHub's web servers render the diagram
graphically when viewed in a web browser.

Support and Licensing
---------------------
Please see [LICENSE.txt] for the copyright and license under which Formal is distributed.
To report any difficulty with building, testing, or using Formal, please submit an [issue].
To contribute code, please submit a [pull request] from a fork of Formal.

Funding Acknowledgment
----------------------
Formal is a software artifact of research funded by the Competitive Portfolios for Advanced
Scientific Computing Research Program of the U.S. Department of Energy, Office of Science,
Office of Advanced Scientific Computing Research under contract DE-AC02-05CH11231.

[`fpm`]: https://github.com/fortran-lang/fpm
[fpm-0.12.0.F90]: https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0.F90
[`doc/uml/class-diagram.md`]: ./doc/uml/class-diagram.md
[Castillo & Corbino (2020)]: https://doi.org/10.1016/j.cam.2019.06.042 
[`example/extended-gauss-divergence.F90`]: ./example/extended-gauss-divergence.F90
[issue]: https://github.com/berkeleylab/formal/issues
[pull request]: https://github.com/berkeleylab/formal/pulls
[LICENSE.txt]: ./LICENSE.txt
