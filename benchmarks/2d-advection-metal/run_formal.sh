#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
    echo "the Formal Metal benchmark requires an Apple Silicon Mac" >&2
    exit 1
fi
if [[ "${FA_ALLOW_METAL:-0}" != 1 ]]; then
    echo "Metal mode is guarded; set FA_ALLOW_METAL=1 to proceed." >&2
    exit 1
fi

source "$here/../toolchain.sh"
benchmark_toolchain "$here/run_formal.sh" "$@"
gpu_guard="$lfortran_root/examples/nbody/gpu_run.sh"
if [[ "$compiler" != /* ]]; then
    echo "LFORTRAN_COMPILER must be an absolute path." >&2
    exit 2
fi
if [[ ! -x "$compiler" ]]; then
    echo "LFortran compiler not found at $compiler; build it with the llvm11 Pixi environment." >&2
    exit 2
fi
if [[ ! -f "$formal_root/fpm.toml" || ! -f "$gpu_guard" ]]; then
    echo "Formal checkout or LFortran GPU guard is incomplete; set LFORTRAN_ROOT explicitly." >&2
    exit 2
fi

benchmark_require_fpm
compiler_hash="$(shasum -a 256 "$compiler" | awk '{print $1}')"
build_dir="$here/build/formal/compiler-${compiler_hash:0:16}"
mkdir -p "$build_dir"
printf 'compiler=%s\ncompiler_sha256=%s\nfpm=%s\nSDK clang=%s\n' \
    "$compiler" "$compiler_hash" "$benchmark_fpm" "$sdk_clang"
"$compiler" --version
"$benchmark_fpm" --version

cd "$formal_root"
"$benchmark_fpm" run --compiler "$compiler" --build-dir "$build_dir" \
    --c-compiler "$sdk_clang" \
    --flag "--cpp --realloc-lhs-arrays --separate-compilation --gpu=metal" \
    --c-flag "-DFORMAL_ENABLE_GPU_BENCHMARK -I$lfortran_root/src/libasr/runtime" \
    --example 2D-advection-diffusion --runner /bin/cp -- "$build_dir/formal.metal"
if ! /usr/bin/otool -L "$build_dir/formal.metal" | grep -q '/Metal.framework/'; then
    echo "FPM did not produce a Metal-linked executable." >&2
    exit 1
fi

if [[ $# -eq 0 ]]; then
    set -- --cells 2048,2048 --steps 1000 --no-csv
fi
export LFORTRAN_GPU_PROFILE=1
GPU_TIMEOUT="${GPU_TIMEOUT:-30}" \
    /bin/bash "$gpu_guard" "$build_dir/formal.metal" --gpu-profile "$@"
printf 'Artifacts: %s\n' "$build_dir"
