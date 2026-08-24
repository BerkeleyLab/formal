```ascii
___________                         .__   
\_   _____/__________  _____ _____  |  |   
 |    __)/  _ \_  __ \/     \\__  \ |  |   
 |     \(  <_> )  | \/  Y Y  \/ __ \|  |__ 
 \___  / \____/|__|  |__|_|  (____  /____/
     \/                    \/     \/          
```

Formal: Formulaic mimetic abstraction language
============================================

_Towards an embedded domain-specific language (DSL) for verifiable vector-calculus computations._

Introduction
------------
Formal supports research on mimetic software abstractions for vector calculus by providing

- Derived types that mimic vector and tensor fields and
- Differential and integral operators for writing vector and tensor expressions.

Formal's types and operators implement the discrete calculus of [Corbino & Castillo (2020)]:
mimetic numerical methods that satisfy discrete versions of vector calculus theorems.

Like the underlying numerical methods, Formal's software abstractions mimic their vector calculus counterparts.
For example, given scalar and vector fields $f$ and $\vec{v}$ defined over a unit volume $V = [0,1]^3$ bounded
by a surface area $A$, the program [`example/extended-gauss-divergence.F90`] demonstrates satisfaction of the
extended Gauss divergence theorem: 

$$ \iiint_V (\vec{v} \cdot \nabla f) dV + \iiint_V (f \nabla \cdot \vec{v}) dV = \iint_A f \vec{v} \cdot d\vec{A} $$

Running the program as follows
```fortran
fpm run --example extended-gauss-divergence --compiler flang --profile release
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

**Future work:** Formal lays a foundation for defining a verifiable embedded DSL using type-safe generic programming.

Examples
--------
### Highlights
Formal now supports 2D and 3D operators that compute the gradient (`.grad.`) of
a scalar field, the divergence (`.div.`) of a vector field, and the arithmetic
operators required for expressing equations such as the advection/diffusion 
partial differential equation (PDE):

$$ \partial s / \partial t = \nabla \cdot (D \nabla s) - \nabla \cdot (\vec{v}s)$$

which Formal facilitates writing as
```
ds_dt = .div. (D * .grad. s) - .div. (v * s)
```
where `s` is the concentration of a passive scalar quantity, `D` is a molecular diffusion
coefficient, and `v` is a prescribed velocity field.  This repository's 2D-advection-diffusion
[program](./example/2D-advection-diffusion.F90), for example, demonstrates how to advance the
above equation in time using a Runge-Kutta scheme.

### Other example programs
See this repository's [example](./example) subdirectory for additional demonstrations of
using Formal.  For each example, obtain usage information f execute something like
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
included among the release assets.  For the `fpm` 0.13.0 release, for example,
compiling [fpm-0.13.0.F90] and placing the resulting executable file in your
`PATH` suffices.

Building and testing
--------------------
### Supported Compilers

 Vendor  | Compiler  | Version(s)| Build/Test Command
---------|-----------|-----------|-------------------
 LFortran| `lfortran`| 0.64      | `fpm test --compiler lfortran --flag "--cpp --realloc-lhs-arrays --separate-compilation"`
 LLVM    | `flang`   | 20-23     | `fpm test --compiler flang --profile release`
 LLVM    | `flang`   | 19        | `fpm test --compiler flang --profile release --flag "-mmlir -allow-assumed-rank"`
 NAG     | `nagfor`  | 7.2       | `fpm test --compiler nagfor --flag "-fpp -O4"`

#### LLVM
With `fpm` Versions before 0.13.0, replace  `flang` with `flang-new` and delete `--profile release` in the tabulated commands above.

#### NAG 
Building with `nagfor` requires an `fpm` version that contains pull request [#1312] and/or that fixes issue [#1313].

### Unsupported Compilers
---------------------
Recent commits exposed issues with the Intel `ifx` and `gfortran` compilers that block building Formal.
Once the issues have been addressed, the corresponding compiler's content will be moved back up to [Supported Compilers] table.

 Vendor| Compiler  |Version |Build/Test Command
-------|-----------|--------|------------------
 Intel | `ifx`     |2026.1.0|`FOR_COARRAY_NUM_IMAGES=1 fpm test --compiler ifx --flag "-fpp -O3 -coarray" --profile release`
 GCC   | `gfortran`|16.2.0  |`fpm test --compiler gfortran --profile release`

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

[#1312]: https://github.com/fortran-lang/fpm/pull/1312
[#1313]: https://github.com/fortran-lang/fpm/issues/1313
[Corbino & Castillo (2020)]: https://doi.org/10.1016/j.cam.2019.06.042
[`doc/uml/class-diagram.md`]: ./doc/uml/class-diagram.md
[`example/extended-gauss-divergence.F90`]: ./example/extended-gauss-divergence.F90
[`fpm`]: https://github.com/fortran-lang/fpm
[fpm-0.13.0.F90]: https://github.com/fortran-lang/fpm/releases/download/v0.13.0/fpm-0.13.0.F90
[issue]: https://github.com/berkeleylab/formal/issues
[LICENSE.txt]: ./LICENSE.txt
[pull request]: https://github.com/berkeleylab/formal/pulls
[Supported Compilers]: #supported-compilers
