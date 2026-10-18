# Native C + Metal performance notes

The native program is a reproducible oracle and a performance reference for
Formal's corrected 2D advection-diffusion workspace.  It uses FP32 mimetic
gradient, interpolation, and divergence operators with classical RK4.  It
does not replace the method with a five-point Laplacian.

## Verified scheduling

The Metal host keeps the verified bounded schedule:

- four RK4 stages per timestep;
- eight timesteps and 64 dispatches per command buffer;
- at most two command buffers in flight;
- an autorelease pool per encoded command buffer;
- serial retirement and unioned command-buffer timestamps for `gpu_busy`.

The CPU implementation is intentionally retained as the oracle.  The
corrected one-sided gradient rows, fixed extended scalar boundaries, and
`k1 + 2*k2 + 2*k3 + k4` accumulation order are shared by the CPU and Metal
implementations.

## Reproduction

On the Apple M4 Max used for the original comparison, the measured Gaussian
case was 2048 x 2048 cells for 1000 steps with the example's scaled timestep
(approximately `6.20126741e-8`).  Run the native reference with:

```bash
LFORTRAN_ROOT=/path/to/lfortran \
  bash benchmarks/2d-advection-metal/run.sh \
  --mode metal --cells 2048,2048 --steps 1000
```

The wrapper compiles with the configured macOS SDK compiler and invokes
LFortran's existing GPU guard directly.  The guard is required for every
Metal run and serializes users through `/tmp/gpurun.lock.d`.
The native program writes no CSV unless `--dump` or `--output` is supplied;
use `--dump-bin` when a full-precision field is needed for comparison.

The original paired measurement used the same raw Formal input for both
implementations:

| variant | solve wall | GPU command-buffer time |
|---|---:|---:|
| Formal + LFortran | 2.195701 s | 2.186008 s |
| native C + Metal | 2.250356 s | 2.247697 s |

Those values are historical verification samples, not a promise of a fixed
runtime across machines.  The native result was within the measured Formal
throughput, with largest elementwise difference
`5.96046448e-8` and relative L2 difference `5.98075916e-9`.

## Index experiment

`--index-mode linear` preserves the signed flattened-index implementation
from the verified source.  On the smaller 257 x 257 cache-resident workload
it measured about 1.55 s GPU versus 1.08 s for direct 2D indexing.  An
unsigned-index experiment was slightly slower and was rejected; this document
does not claim that unsigned indexing is faster.

## Provenance

The native source and shader are from LFortran commit
`3d4c7941c5a153adb996ed62309225e871c06d9b`; see the benchmark
[`README.md`](./README.md) and [`LFortran-NOTICE.txt`](./LFortran-NOTICE.txt)
for the exact source paths and license handling.
