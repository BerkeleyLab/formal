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
(approximately `6.20126741e-8`). The native variant used direct 2D indexing,
**128 x 1 threadgroups**, and the **same raw Formal input**, not its default
32 x 4 threadgroups or independently generated analytic input.

Run the following from the Formal repository root on an Apple Silicon Mac,
in one Bash session. Set `LFORTRAN_ROOT` to an absolute checkout with its
LLVM 11 Pixi compiler already built. Metal permission is deliberately
explicit; neither wrapper enables it automatically.

```bash
set -euo pipefail
export LFORTRAN_ROOT=/path/to/lfortran
export FA_ALLOW_METAL=1 GPU_TIMEOUT=30 GPU_MAXMEM_MB=4096
nx=2048
ny=2048
steps=1000
pair_dir="$PWD/benchmarks/2d-advection-metal/build/paired-${nx}x${ny}-${steps}"
mkdir -p "$pair_dir"

pixi run --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 \
  bash benchmarks/2d-advection-metal/run_formal.sh \
  --solver workspace --order 4 --cells "$nx,$ny" --steps "$steps" --no-csv \
  --dump-input "$pair_dir/input.fadv2d" \
  --dump-final-raw "$pair_dir/formal-final.fadv2d" \
  > "$pair_dir/formal.log" 2>&1

pixi run --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 \
  bash benchmarks/2d-advection-metal/run.sh \
  --mode metal --input "$pair_dir/input.fadv2d" \
  --index-mode 2d --tg 128,1 --dump-bin "$pair_dir/native-final.bin" \
  > "$pair_dir/native.log" 2>&1

cat "$pair_dir/formal.log" "$pair_dir/native.log"
```

Formal exports the initial fields before warmup or solve timing. Native
`--input` reads those fields and their exact grid, step count, and FP32
timestep from the snapshot; do not regenerate the input or round the
timestep. Absolute artifact paths are important: the Formal wrapper runs
from the repository root, while the native wrapper runs from this directory.
Both wrappers pass the actual executable directly to LFortran's existing
GPU guard. The guard is required for every Metal run and serializes users
through `/tmp/gpurun.lock.d`.
Check that Formal reports `gpu_timing_available=T` and positive
`gpu_busy_seconds`, and native reports `mode=metal`, `index_mode=2d`,
`tg=128,1`, and positive `gpu_busy`. These solve timings exclude compilation,
initialization, warmup/JIT, and field I/O. Neither command writes CSV.

For a bounded non-square command-wiring check, use `nx=17`, `ny=13`, and
`steps=9` above.
That is a reduced check, not a reproduction of the historical timings.

To validate the complete FP32 fields, use the same shell variables and the
shared raw/field comparator below. It reads the initial scalar and velocity
payloads as well as both finals, checks their sizes and finiteness, and
requires matching grid, step count, and FP32 timestep metadata. Fixed scalar
boundaries must equal the initial values exactly. Final-field agreement uses
the workspace validator's tolerances (`rtol=1e-6`, `atol=1e-8`) at every point,
including boundaries, and reports the relative L2 error.

For this nonstationary Gaussian, **keep `--evolution`**: both final fields
must be nonzero and must differ meaningfully from the shared initial field.
The maximum change must exceed `atol + rtol * max(abs(initial))`, and the
relative L2 change must exceed `rtol`. This rejects unchanged initial fields,
zeroed finals, and roundoff-only changes even when the two finals agree.
The option is deliberately not a requirement for legitimate stationary or
zero-state tests. Positive GPU time establishes execution, not numerical
correctness; both telemetry and field validation are required.

```bash
pixi run --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 \
  python example/scripts/compare_2d_raw_fields.py \
  "$pair_dir/input.fadv2d" "$pair_dir/formal-final.fadv2d" \
  "$pair_dir/native-final.bin" --cells "$nx,$ny" --steps "$steps" \
  --rtol 1e-6 --atol 1e-8 --evolution > "$pair_dir/compare.log" 2>&1
cat "$pair_dir/compare.log"
```

For small cases, `--cpu-final <native-CPU-payload>` additionally checks both
finals against the retained native CPU oracle, with the same tolerances,
evolution requirement, and fixed-boundary checks. Comparator regressions can
be checked without a GPU:

```bash
mkdir -p benchmarks/2d-advection-metal/build
pixi run --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 \
  python example/scripts/compare_2d_raw_fields.py --self-test \
  > benchmarks/2d-advection-metal/build/compare-selftest.log 2>&1
cat benchmarks/2d-advection-metal/build/compare-selftest.log
```

These synthetic comparator tests are not solver validation.

## Historical paired results

The original measurement used three interleaved pairs with the configuration
above. These are the historical medians, not results from the reduced
command-wiring check:

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
