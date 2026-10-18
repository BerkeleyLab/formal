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

The wrappers use the LFortran `llvm11` Pixi environment.  They accept an
explicit `LFORTRAN_ROOT`; otherwise they look for a sibling checkout at
`../lfortran` and fail with an actionable message if it is absent:

```bash
LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run.sh
```

The wrapper re-enters Pixi with
`pixi run --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11`, selects the
configured macOS SDK compiler with `xcrun`, and builds
`build/formal_advection`.  With no arguments it runs a small CPU smoke case.
The native executable reads `formal_advection.metal` from its current working
directory; `run.sh` changes to this directory before invoking it, so it cannot
accidentally load a stale shader.

Metal is opt-in and doubly guarded:

```bash
FA_ALLOW_METAL=1 LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run.sh \
  --mode metal --cells 257,257 --steps 30000
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

To run the actual Formal GPU example rather than the native oracle:

```bash
LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run_formal.sh
```

That wrapper compiles the existing
`example/2D-advection-diffusion.F90` with LFortran, checks Metal linkage, and
uses the same guarded launch path.  It does not add native sources to FPM.

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
