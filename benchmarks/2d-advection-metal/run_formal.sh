#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
formal_root="$(cd "$here/../.." && pwd)"
lfortran_root="${LFORTRAN_ROOT:-"$formal_root/../lfortran"}"

if [[ ! -f "$lfortran_root/pixi.toml" ]]; then
    echo "LFortran checkout not found at $lfortran_root; set LFORTRAN_ROOT to its absolute path." >&2
    exit 2
fi
lfortran_root="$(cd "$lfortran_root" && pwd)"
if [[ "${PIXI_PROJECT_ROOT:-}" != "$lfortran_root" ||
      "${PIXI_ENVIRONMENT_NAME:-}" != llvm11 ]]; then
    exec pixi run --manifest-path "$lfortran_root/pixi.toml" -e llvm11 \
        bash "$here/run_formal.sh" "$@"
fi

if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
    echo "the Formal Metal benchmark requires an Apple Silicon Mac" >&2
    exit 1
fi
if [[ "${FA_ALLOW_METAL:-0}" != 1 ]]; then
    echo "Metal mode is guarded; set FA_ALLOW_METAL=1 to proceed." >&2
    exit 1
fi

compiler="${LFORTRAN_COMPILER:-${LFORTRAN_BUILD_DIR:-"$lfortran_root/build/llvm11"}/src/bin/lfortran}"
gpu_guard="$lfortran_root/examples/nbody/gpu_run.sh"
if [[ ! -x "$compiler" ]]; then
    echo "LFortran compiler not found at $compiler; build it with the llvm11 Pixi environment." >&2
    exit 2
fi
if [[ ! -f "$formal_root/fpm.toml" || ! -f "$gpu_guard" ]]; then
    echo "Formal checkout or LFortran GPU guard is incomplete; set LFORTRAN_ROOT explicitly." >&2
    exit 2
fi

compiler_hash="$(shasum -a 256 "$compiler" | awk '{print $1}')"
build_dir="$here/build/formal/compiler-${compiler_hash:0:16}"
mkdir -p "$build_dir"

cd "$formal_root"
fpm run --compiler "$compiler" --build-dir "$build_dir" \
    --flag "--cpp --realloc-lhs-arrays --separate-compilation --gpu=metal" \
    --c-flag "-DFORMAL_ENABLE_GPU_BENCHMARK -I$lfortran_root/src/libasr/runtime" \
    --example 2D-advection-diffusion --runner cp -- "$build_dir/formal.metal"
if ! otool -L "$build_dir/formal.metal" | grep -q '/Metal.framework/'; then
    echo "FPM did not produce a Metal-linked executable." >&2
    exit 1
fi

if [[ $# -eq 0 ]]; then
    set -- --cells 2048,2048 --steps 1000 --no-csv
fi
export LFORTRAN_GPU_PROFILE=1
GPU_TIMEOUT="${GPU_TIMEOUT:-30}" \
    bash "$gpu_guard" "$build_dir/formal.metal" --gpu-profile "$@"
printf 'Artifacts: %s\n' "$build_dir"
