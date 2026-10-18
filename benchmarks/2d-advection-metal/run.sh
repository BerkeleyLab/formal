#!/usr/bin/env bash
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$(uname -s)" != Darwin || "$(uname -m)" != arm64 ]]; then
    echo "the native Metal benchmark requires an Apple Silicon Mac" >&2
    exit 1
fi

if [[ $# -eq 0 ]]; then
    set -- --mode cpu --cells 11,11 --steps 2 --dt 1e-4
fi

mode=cpu
mode_seen=0
expect_mode=0
for arg in "$@"; do
    if ((expect_mode)); then
        mode="$arg"
        expect_mode=0
    elif [[ "$arg" == "--mode" ]]; then
        if ((mode_seen)); then
            echo "--mode may be specified only once" >&2
            exit 2
        fi
        expect_mode=1
        mode_seen=1
    fi
done
if ((expect_mode)); then
    echo "--mode must be followed by cpu or metal" >&2
    exit 2
fi
if [[ "$mode" != cpu && "$mode" != metal ]]; then
    echo "--mode must be cpu or metal" >&2
    exit 2
fi
if [[ "$mode" == metal && "${FA_ALLOW_METAL:-0}" != 1 ]]; then
    echo "Metal mode is guarded; set FA_ALLOW_METAL=1 to proceed." >&2
    exit 1
fi

source "$here/../toolchain.sh"
benchmark_toolchain "$here/run.sh" "$@"
gpu_guard="$lfortran_root/examples/nbody/gpu_run.sh"
if [[ ! -f "$gpu_guard" ]]; then
    echo "LFortran GPU guard not found at $gpu_guard; use a complete LFortran checkout." >&2
    exit 2
fi

build_dir="$here/build"
binary="$build_dir/formal_advection"
mkdir -p "$build_dir"
"$sdk_clang" -Wall -Wextra -Werror -O2 -fobjc-arc -isysroot "$sdkroot" \
    -framework Foundation -framework Metal \
    "$here/formal_advection.m" -o "$binary"

cd "$here"
if [[ "$mode" == metal ]]; then
    exec /bin/bash "$gpu_guard" "$binary" "$@"
fi
exec "$binary" "$@"
