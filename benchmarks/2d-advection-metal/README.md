# Native 2D advection-diffusion reference

This directory contains the native C + Metal reference implementation used
to measure the Formal 2D advection-diffusion example.  It is deliberately
under `benchmarks/`, outside FPM's automatic `example/` discovery, so the
Objective-C `main` is never added to ordinary Formal examples or tests.

The CPU path is the numerical oracle.  Both paths implement the corrected
order-4 mimetic gradient, center-to-face interpolation, and divergence
closures followed by classical RK4:

```text
rhs = div(D * grad(s)) - div(v * interp(s))
```

This is not a five-point Laplacian or a lower-order substitute.  Scalar
boundaries remain fixed, the FP32 field layout matches Formal, and the RK4
accumulator is updated in the same `k1 + 2*k2 + 2*k3 + k4` order.

## Build and run

The optional benchmark tooling uses two managed environments:

- an already-built LFortran checkout's `llvm11` environment for its compiler,
  Python and GFortran;
- [`../pixi.toml`](../pixi.toml) and [`../pixi.lock`](../pixi.lock) for
  **FPM 0.13.0** and its runtime dependencies. Clean LFortran main does not
  declare FPM. This small, Apple-Silicon-only benchmark environment does not
  change ordinary Formal/FPM library use or the supported-compiler CI matrix.

Install Pixi and the real Xcode Command Line Tools first. The wrappers accept
an explicit `LFORTRAN_ROOT`; otherwise they look for a sibling checkout at
`../lfortran` and fail with an actionable message if it is absent:

```bash
LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run.sh
```

The wrappers re-enter
`pixi run --locked --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11`.
Their shared [`../toolchain.sh`](../toolchain.sh) selects the **real SDK
compiler after activation**, through a child-process `PATH`, `CC`, and
linker settings. This matters on clean LLVM 11 main: its Metal runtime invokes
`clang` through `PATH`; setting `CC` alone leaves Pixi's old Clang compiling
new Foundation headers and fails. There is no fake clang wrapper, compiler
patch, or change to the LFortran manifest.

The native wrapper builds `build/formal_advection`; it does not need FPM.
With no arguments it runs a small CPU smoke case.
The native executable reads `formal_advection.metal` from its current working
directory; `run.sh` changes to this directory before invoking it, so it cannot
accidentally load a stale shader.

Metal is opt-in and doubly guarded:

```bash
FA_ALLOW_METAL=1 LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run.sh \
  --mode metal --cells 17,13 --steps 9 --index-mode 2d --tg 128,1
```

Metal runs invoke the existing LFortran
`examples/nbody/gpu_run.sh` directly.  That preserves its shared
`/tmp/gpurun.lock.d` serialization, timeout, and memory guard; there is no
unguarded fallback.  `GPU_TIMEOUT` and `GPU_MAXMEM_MB` are passed through to
that guard.

Useful native options include `--dump <path>`, `--dump-bin [path]`,
`--index-mode 2d|linear`, and `--tg x[,y]`.  The `linear` mode is the signed
flattened-index experiment.  The verified comparison found it slower on the
small cache-resident case; no claim that unsigned indexing is faster is made.

To run the actual Formal GPU example rather than the native oracle, the same
Apple Silicon and explicit Metal opt-in requirements apply:

```bash
FA_ALLOW_METAL=1 LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run_formal.sh
```

That wrapper rejects missing permission or an unsupported platform before
compilation. It defaults to 2048 x 2048 cells and 1000 steps, compiles
`example/2D-advection-diffusion.F90` with LFortran, checks Metal linkage, and
uses the same guarded launch path.  It does not add native sources to FPM.
It installs the locked benchmark FPM environment automatically (or run
`pixi install --locked --manifest-path benchmarks/pixi.toml` explicitly).
FPM and Fortran are invoked by absolute managed paths, so selecting the SDK
does not accidentally select host FPM/Fortran copies. The default compiler is
`$LFORTRAN_ROOT/build/llvm11/src/bin/lfortran`; an explicit
`LFORTRAN_COMPILER` must be an absolute path. The build log records its version,
SHA-256, FPM path/version and SDK compiler. Julienne 4.1.1 is still fetched
through the unchanged `fpm.toml`.
See [`PERFORMANCE.md`](./PERFORMANCE.md#reproduction) for the paired
shared-input recipe and the explicit native launch geometry used in the
historical comparison.

Clean-main compile/link, CPU checks and ten guarded GPU executions have now
passed with this tooling. The substantial 2048²/1000 Gaussian comparison
measured median solve walls of **2.167132 s Formal / 2.188441 s native**,
with full-field, evolution and fixed-boundary validation. These are fresh
clean-main results, not merely a successful Metal link. See the
[paired GPU measurements](./PERFORMANCE.md#clean-main-paired-gpu-results) and
[controlled CPU measurements](./PERFORMANCE.md#controlled-cpu-measurements)
for the measured, workload-specific coefficient and workspace effects.

## Known Fortran compiler limitations

The standalone C/Metal reference is independent of the Fortran compiler.
The Fortran example has known compiler-version failures in
[the combined draft's initial CI](https://github.com/BerkeleyLab/formal/actions/runs/39135296334):

- CI's Flang 21 crashes while lowering `do concurrent ... local(rhs, total)`.
- LFortran 0.64–0.66 fail LLVM type verification for `numerical_arguments_t`
  while compiling the example's argument and raw-input helpers.

Both failures occur before the solver runs. Current Linux LFortran builds and
the other completed Flang configurations compile the portable example.
Historical and fresh clean-main GPU comparisons verify evolved full fields
and unchanged boundaries. These older
compiler limitations are recorded without changing the numerical method or
disabling their CI checks.

The initial combined-draft CI passes 11 of 17 compiler checks, plus its
metadata check. Its macOS 15/26 LFortran 0.67 bottles require
`libz3.5.1.dylib`, but the installed Z3 is 5.3.1: even `lfortran --version`
fails, before Formal compilation. Linux LFortran 0.67/current and the other
Flang checks pass. These packaging failures, the older branch's macOS 14 Z3
build failure, and its Intel macOS jobs cancelled during LLVM installation
do not validate those platforms. All remain document-only limitations:
no version drops, workflow repairs, test skips, or numerical workarounds.

## Provenance and licensing

The numerical algorithm, FP32 CPU oracle, Metal kernels, two-deep scheduling,
and signed-index experiment come from the LFortran repository at commit
`3d4c7941c5a153adb996ed62309225e871c06d9b`
(`examples/formal_advection/formal_advection.m` and
`examples/formal_advection/formal_advection.metal`).  The provenance of each
native source file is recorded separately:

- `formal_advection.metal` is byte-for-byte identical to the LFortran shader.
  Its original and current Git blob hashes are both
  `0e492038ab56e7fbb70789b724e934093392c3b0`.
- `formal_advection.m` retains that source's numerical method and CPU oracle,
  but is not byte-for-byte identical.  The Formal copy adds checked
  `size_t` addition/multiplication and a 512 MiB native working-set guard
  covering the host fields, warm-up backups, and Metal buffers.  Its original
  LFortran blob hash is `a2d79a6af54df22e4af9c4bffa85d5712942a2c8`; the
  current Formal destination blob hash is
  `13083cc9ab9547064b0d668552d747c1b8b203c2`.

These C-side safety additions do not alter the numerical method or benchmark
claims.  The adapted wrappers and this documentation are the Formal
publication integration.  The upstream LFortran BSD terms are retained in
[`LFortran-NOTICE.txt`](./LFortran-NOTICE.txt); the surrounding Formal tree
remains covered by [`LICENSE.txt`](../../LICENSE.txt).

Generated binaries, logs, CSV/raw fields, and build caches belong under
`build/` and are ignored.  No Fortran workspace fixture is duplicated here:
the Formal implementation remains the tracked
`example/2D-advection-diffusion.F90`.
