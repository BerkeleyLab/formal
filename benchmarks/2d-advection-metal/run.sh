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
        bash "$here/run.sh" "$@"
fi

if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
    echo "the native Metal benchmark requires an Apple Silicon Mac" >&2
    exit 1
fi

gpu_guard="$lfortran_root/examples/nbody/gpu_run.sh"
if [[ ! -f "$gpu_guard" ]]; then
    echo "LFortran GPU guard not found at $gpu_guard; use a complete LFortran checkout." >&2
    exit 2
fi

build_dir="$here/build"
binary="$build_dir/formal_advection"
mkdir -p "$build_dir"

if [[ -n "${CC:-}" ]]; then
    sdk_clang="$CC"
else
    sdk_clang="$(xcrun --sdk macosx --find clang)"
fi
sdkroot="${SDKROOT:-$(xcrun --sdk macosx --show-sdk-path)}"
"$sdk_clang" -Wall -Wextra -Werror -O2 -fobjc-arc -isysroot "$sdkroot" \
    -framework Foundation -framework Metal \
    "$here/formal_advection.m" -o "$binary"

if [[ $# -eq 0 ]]; then
    set -- --mode cpu --cells 11,11 --steps 2 --dt 1e-4
fi

mode=cpu
mode_seen=0
for ((i = 1; i <= $#; ++i)); do
    if [[ "${!i}" == "--mode" ]]; then
        if ((i == $#)); then
            echo "--mode must be followed by cpu or metal" >&2
            exit 2
        fi
        if ((mode_seen)); then
            echo "--mode may be specified only once" >&2
            exit 2
        fi
        j=$((i + 1))
        mode="${!j}"
        mode_seen=1
    fi
done
if [[ "$mode" != cpu && "$mode" != metal ]]; then
    echo "--mode must be cpu or metal" >&2
    exit 2
fi

cd "$here"
if [[ "$mode" == metal ]]; then
    if [[ "${FA_ALLOW_METAL:-0}" != 1 ]]; then
        echo "Metal mode is guarded; set FA_ALLOW_METAL=1 to proceed." >&2
        exit 1
    fi
    exec bash "$gpu_guard" "$binary" "$@"
fi
exec "$binary" "$@"
