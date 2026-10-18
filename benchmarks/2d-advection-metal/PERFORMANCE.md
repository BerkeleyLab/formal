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
The Formal wrapper provisions FPM 0.13.0 from the benchmark-local Pixi
manifest/lockfile; it does not assume FPM exists in LFortran's environment.
Both wrappers select the real SDK compiler **after** Pixi activation.

```bash
set -euo pipefail
export LFORTRAN_ROOT=/path/to/lfortran
export LFORTRAN_COMPILER="$LFORTRAN_ROOT/build/llvm11/src/bin/lfortran"
export FA_ALLOW_METAL=1 GPU_TIMEOUT=30 GPU_MAXMEM_MB=4096
nx=2048
ny=2048
steps=1000
pair_dir="$PWD/benchmarks/2d-advection-metal/build/paired-${nx}x${ny}-${steps}"
mkdir -p "$pair_dir/scratch"
export TMPDIR="$pair_dir/scratch"

bash benchmarks/2d-advection-metal/run_formal.sh \
  --solver workspace --order 4 --cells "$nx,$ny" --steps "$steps" --no-csv \
  --dump-input "$pair_dir/input.fadv2d" \
  --dump-final-raw "$pair_dir/formal-final.fadv2d" \
  > "$pair_dir/formal.log" 2>&1

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
  "$LFORTRAN_ROOT/.pixi/envs/llvm11/bin/python" example/scripts/compare_2d_raw_fields.py \
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
  "$LFORTRAN_ROOT/.pixi/envs/llvm11/bin/python" example/scripts/compare_2d_raw_fields.py --self-test \
  > benchmarks/2d-advection-metal/build/compare-selftest.log 2>&1
cat benchmarks/2d-advection-metal/build/compare-selftest.log
```

These synthetic comparator tests are not solver validation.

## Controlled CPU measurements

Measured on **18 October 2026, Apple M4 Max, 64 GiB**, macOS 15.8
(Darwin 24.6), AC power, `powermode=0`. Timed processes used
`OMP_NUM_THREADS=OPENBLAS_NUM_THREADS=VECLIB_MAXIMUM_THREADS=1`; builds used
at most four jobs. This was a shared desktop, without core affinity or locked
clocks: load averages were about 2.4–2.5 during the sequential CPU run.
The CPU has 12 performance and four efficiency cores; no core class was forced.
Three samples are descriptive measurements, not a statistical significance
test or a claim of a universal maximum.

The optimized numerical sources are unchanged from `adf06916`.
Two isolated **source-only** snapshots of the combined branch were used:
the control differs only in these files, restored to correctness prerequisite
`363f8999` (their contents also match the parent of `3866131`):

```text
src/formal/differential_operators_1D_m.F90
src/formal/divergence_operator_1D_s.F90
src/formal/gradient_operator_1D_s.F90
```

No compiled modules, build caches or environments were copied. Both used the
same instrumented example and original `--solver operators`; the workspace
comparison then used the **same folded executable** with `--solver workspace`.
All inputs within each compiler/workload were byte-identical, including
velocities, grid, step count and FP32 timestep. Raw `--input` is workspace-only,
so the operator controls used the same analytic parameters and compared their
exported raw inputs, rather than passing an unsupported option.

Toolchains and matching flags:

- **GFortran 16.2.0**, from the clean LFortran `llvm11` Pixi environment:
  FPM `--profile release --flag "-fcoarray=single -B/usr/bin/ -ffree-line-length-none"`.
  The release profile adds `-O3 -Wimplicit-interface -fPIC -fmax-errors=1
  -funroll-loops`; it does not enable `-ffast-math`.
- **Clean LFortran `2247d35082ea190408486b93c12ec2ead4771735`**, version
  `0.67.0-922-g2247d35082`, LLVM 11.1.0:
  `--flag "--cpp --realloc-lhs-arrays --separate-compilation --fast"`,
  **without** FPM `--profile release`. FPM 0.13.0's LFortran release profile
  emitted a literal `flag_lfortran_opt`, which the linker rejected. The
  explicit `--fast` invocation avoids that tooling defect without changing
  FPM or the compiler. Compiler SHA-256:
  `8a1064336ee4922499cb81674c1fc04fbdd376a4b43a8388d993b8e775f22f8d`.
  This is an unpatched Debug compiler built with `-g -O2` and assertions;
  those compiler-build flags are distinct from the generated program's `--fast`.
- FPM **0.13.0** from Formal's benchmark lockfile; real Apple Clang 17 and
  SDK 26.2 for C/linking; Python 3.12/NumPy 1.26.4 from clean LFortran's Pixi.

One calibration at 51×51/500, `dt=1e-4`, selected the following **frozen**
step counts to keep solves around 0.25–2 s. Each invocation performs the
unchanged untimed one-step warmup and restores the initial state. Three
interleaved triplets used original/folded/workspace, then the reverse order,
then original/folded/workspace. Solve times exclude build, initial setup,
warmup and file I/O; the workspace's once-per-advance velocity scratch
allocation/copy is still inside the solve.

| Compiler / workload | Original operators, median [min, max] s | Folded operators, median [min, max] s | Folded workspace, median [min, max] s |
|---|---:|---:|---:|
| GFortran, 51×51 / 2000 steps | 0.769787 [0.769689, 0.770542] | 0.769291 [0.762533, 0.787986] | 0.264657 [0.263707, 0.265147] |
| LFortran `--fast`, 51×51 / 1500 steps | 1.865753 [1.855085, 1.875574] | 1.866891 [1.850986, 1.872441] | 0.262738 [0.262624, 0.264812] |

**Coefficient-only original/folded median ratios: 1.0006× GFortran,
0.9994× LFortran.** The approximately ±0.06% differences are smaller than the
observed sample spread; these measurements establish neither a meaningful
CPU benefit nor a CPU regression. The separate folded-operators/workspace
ratios were **2.9067×** and **7.1055×**, respectively, on these cases.
Coefficient folding is CPU-neutral within this sample spread; the workspace
ratios compare workspace against **folded operators**, not old versus folded
coefficients.
Do not compare absolute compiler speeds or CPU/GPU times across their
different step counts/workloads.

Every original/folded pair had **byte-identical final fields**. Every
workspace/operator pair passed the unchanged full-field tolerances
(`rtol=1e-6`, `atol=1e-8`): maximum absolute / relative L2 differences were
`2.98e-8 / 6.64e-8` (GFortran) and `5.96e-8 / 6.90e-8` (LFortran).
All fields were finite, evolved beyond the comparator tolerances, and
preserved the initial scalar boundaries exactly. Initial setup medians
were only 87–90 µs (GFortran) and 163–175 µs (LFortran); this is the driver's
whole setup region, **not** an isolated coefficient-constructor microbenchmark.

For an equivalent CPU build from a source snapshot, use absolute tools and
apply SDK selection to the child **after** activation:

```bash
# From the chosen source snapshot; use a different build directory per variant.
export LFORTRAN_ROOT=/absolute/path/to/clean/lfortran
export LFORTRAN_COMPILER="$LFORTRAN_ROOT/build/llvm11/src/bin/lfortran"
export FORMAL_BENCHMARK_ROOT=/absolute/path/to/formal
unset FPM_FC FPM_FFLAGS FPM_CC FPM_CFLAGS FPM_CXX FPM_CXXFLAGS FPM_AR FPM_LDFLAGS
pixi install --locked --manifest-path "$FORMAL_BENCHMARK_ROOT/benchmarks/pixi.toml"
fpm="$FORMAL_BENCHMARK_ROOT/benchmarks/.pixi/envs/default/bin/fpm"
cpu_build="$FORMAL_BENCHMARK_ROOT/benchmarks/2d-advection-metal/build/cpu-folded-lfortran"
mkdir -p "$cpu_build/scratch"
export TMPDIR="$cpu_build/scratch" OMP_NUM_THREADS=4

pixi run --locked --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 \
  /usr/bin/env PATH="/usr/bin:/bin:$LFORTRAN_ROOT/build/llvm11/src/bin:$LFORTRAN_ROOT/.pixi/envs/llvm11/bin" \
  LFORTRAN_CC=/usr/bin/clang LFORTRAN_LINKER_PATH=/usr/bin \
  "$fpm" run --compiler "$LFORTRAN_COMPILER" --c-compiler /usr/bin/clang \
  --build-dir "$cpu_build" \
  --flag "--cpp --realloc-lhs-arrays --separate-compilation --fast" \
  --example 2D-advection-diffusion --runner /bin/cp -- "$cpu_build/formal.cpu" \
  > "$cpu_build/build.log" 2>&1

pixi run --locked --manifest-path "$LFORTRAN_ROOT/pixi.toml" -e llvm11 \
  /usr/bin/env OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 \
  "$cpu_build/formal.cpu" --solver operators --order 4 --cells 51,51 \
  --steps 1500 --dt 1e-4 --no-csv --dump-input "$cpu_build/input.fadv2d" \
  --dump-final-raw "$cpu_build/final.fadv2d" > "$cpu_build/run.log" 2>&1
```

For GFortran use the absolute
`$LFORTRAN_ROOT/.pixi/envs/llvm11/bin/gfortran`, its release flags above, and
2000 steps. Use the same flags and frozen inputs for the coefficient control.
Repeat with `--solver workspace` in the folded executable, interleave the
three variants and compare the complete exported fields. Neither building
nor a single wall-clock sample alone reproduces the controlled comparison.

### Tooling and CPU validation

The managed native CPU wrapper, missing-permission/dependency/mode
preflights, raw comparator's 54 checks in both normal Python and `-O`, CSV
self-test, and the clean LFortran `--fast` FPM suite (**32/32, zero skipped**)
passed. The actual Formal wrapper compiled and linked a Metal executable
with the clean compiler and real SDK. Before runtime validation, its real
guard's invalid-timeout preflight was tested **before lock acquisition**,
without GPU execution; Metal/Foundation linkage and embedded kernels were
separately verified. The subsequent guarded executions are reported below.

Shared-input CPU checks on nonsquare Gaussian 17×13/9 and nonzero, varying
boundary 13×11/3 cases passed against the unchanged native CPU oracle with
both managed Fortran compilers, including meaningful evolution and exact
boundaries. These checks were **CPU-only**; the following section records
the separate, subsequently completed GPU executions.

## Clean-main paired GPU results

Measured on **18 October 2026**, using Formal source/tooling head
`70e73ee51f72ebaca16544d834bdfd98a9da66a4` and the unpatched clean LFortran
`2247d35082ea190408486b93c12ec2ead4771735` / LLVM 11.1.0 compiler identified
above. The machine was the same M4 Max/macOS 15.8 system: AC power,
`powermode=0`, no core affinity or clock locking, load averages about
2.64–2.66. The coordinator ran the prepared procedure successfully; this
documentation follow-up only audited the saved artifacts, without rerunning
any solver or benchmark.

All **ten GPU executions** passed the unchanged shared guard, with the
actual executable immediately after it, a 30 s timeout and 4096 MiB RSS
limit per executable. The two small Formal/native pairs ran first:

- **Gaussian 17×13/9**, shared raw input: Formal/native maximum difference
  `2.32830644e-10`; each GPU final agreed with the native CPU oracle to
  `7.45058060e-9` maximum absolute difference.
- **Nonzero, varying boundary 13×11/3**: Formal, native GPU and CPU-oracle
  finals were identical. All three evolved meaningfully, rather than
  merely returning identical initial fields.

Both small pairs reported positive completed GPU time and passed full-field
finiteness, evolution and exact fixed-boundary checks at the unchanged
`rtol=1e-6`, `atol=1e-8`.

The substantial case was the **actual Gaussian, 2048×2048 cells, 1000 steps**,
with exact FP32 `dt=6.20126741068816e-8`, one shared raw input, and native
**direct 2D / 128×1** geometry. The first Formal run exported that input;
the later Formal runs and every native run consumed the same immutable
snapshot. Three interleaved pairs ran Formal/native, native/Formal,
Formal/native. Each executable performed an untimed one-step warmup and
restored its initial state before solve timing.

| Implementation | Median solve wall [min, max], s | Median completed GPU busy [min, max], s |
|---|---:|---:|
| Formal + clean-main LFortran workspace | **2.167132** [2.161986, 2.167841] | **2.158323** [2.152374, 2.159060] |
| Native C + Metal | **2.188441** [2.184408, 2.203517] | **2.187208** [2.183102, 2.202095] |

Native/Formal median ratios are **1.00983× wall** and **1.01338× GPU busy**.
Both implementations therefore solve this case in about 2.2 s; this is
workload/toolchain-specific evidence, not a universal speed or occupancy
limit. Timings exclude compilation, initialization, warmup/JIT and field I/O.
All six large solves stayed in the specified 1–5 s window.

Every large pair passed the complete field comparison: maximum absolute
difference **`5.96046448e-8`**, relative L2 difference
**`5.98075916e-9`**, finite values and exactly preserved initial boundaries.
Both implementations changed the scalar by about `4.17232513e-4` in max norm
and `2.8552e-4` in relative L2, well beyond the evolution thresholds.
Within each implementation, final files were byte-identical across all
three repetitions; Formal and native finals were not byte-identical to
each other. The large case was compared between GPU implementations;
CPU-oracle validation was performed on the two bounded small cases.

Formal reported the following **observed runtime counters in each large
solve**, not just the expected `8 * steps` stage-kernel count:

| Counter | Per solve |
|---|---:|
| Dispatches | 8,003 |
| Commits / completed command buffers | 126 / 126 |
| Host syncs / waits | 2 / 124 |
| Host-to-device / device-to-host memcpy bytes | 201,654,400 / 117,637,200 |
| Buffer allocations / allocated bytes | 5 / 84,017,200 |
| JIT time inside solve | 0 s |

These byte counts describe the runtime's logical copies, **not physical
DRAM traffic measurements**. Five runtime allocations also preclude an
allocation-free claim.

Native currently reports wall time and completed-command-buffer `gpu_busy`,
not dynamic dispatch/copy counters. Its unchanged source schedules eight
dispatches per step, eight steps per commit, and at most two command buffers
in flight: 8,000 dispatches, 125 commits/waits and a 16,810,000-byte final
shared-buffer-to-host copy for this case. These native counts are
**source-derived**, not measured telemetry.

Identities of the measured artifacts (not guarantees of bit-identical builds
on other machines):

```text
Formal Metal binary SHA256:
3b0f83ac19fb8858cb59863dffd5da2c882ccdcb801430f6007b008943485668
Native binary SHA256:
7873f33da20935ab50dae00c5a319d0f43301d1f837d54d37804b9eca2de485d
Shared 2048x2048/1000 raw input SHA256:
ad304f4fce24f392965f4bca34f6d74f8a57570da185d2547537db91cdefbfc7
```

Source/binary/guard/input identities and every reported sample were audited
against the saved execution and comparison logs. No historical binary or
old result was substituted.

## Performance-portability interpretation

The coefficient change evaluates `Ap(i,j) = -A(m+1-i,n+1-j)` in one
assignment instead of two passes. It preserves values, allocation shape and
Fortran storage order; it is not a transpose or physical layout switch.
Its CPU cost is separate from workspace allocation reuse and GPU launch
reduction. Likewise, LFortran #15506 maps adjacent GPU threads to the
contiguous subscript without requiring a Fortran array-layout change.

Unmeasured cases include other CPUs/compilers, larger sustained CPU grids,
isolated constructor costs, and GPU/native parity on other workloads. A future
automatic representation choice would need to preserve storage association,
array element order, C interoperability and observable layout (`SEQUENCE`,
`BIND(C)`, `EQUIVALENCE`, `TRANSFER`, C pointers). No such extension,
conditional CPU implementation or speculative CPU fix is included.
Bandwidth percentages from an assumed bytes-per-cell model, if discussed,
are not DRAM-counter measurements or evidence of hardware occupancy.

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
The compiler was snapshot `3d4c7941`, which included the now-merged GPU work
and an **unmerged driver-selection patch**. In particular, 2.195701 s is not
a fresh result from clean merged main. The earlier coefficient-only GPU
51²/500 comparison, 9.603519 → 4.057951 s (2.3666×), is also historical;
it was not rerun for this study. The fresh table above establishes clean-main
application behavior; the difference from the historical table is not a
controlled old/new compiler speedup comparison.

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
