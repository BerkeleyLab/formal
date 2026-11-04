#!/usr/bin/env bash

# Source this helper, then call benchmark_toolchain with the entry script and arguments.
benchmark_toolchain() {
    local entry_script="$1"
    shift
    formal_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    lfortran_root="${LFORTRAN_ROOT:-"$formal_root/../lfortran"}"
    if [[ ! -f "$lfortran_root/pixi.toml" ]]; then
        echo "LFortran checkout not found at $lfortran_root; set LFORTRAN_ROOT to its absolute path." >&2
        exit 2
    fi
    lfortran_root="$(cd "$lfortran_root" && pwd)"
    benchmark_pixi="$(command -v pixi)" || {
        echo "Pixi is required for the benchmark tools; install Pixi before running this script." >&2
        exit 2
    }
    if [[ "${PIXI_PROJECT_ROOT:-}" != "$lfortran_root" ||
          "${PIXI_ENVIRONMENT_NAME:-}" != llvm11 ]]; then
        export LFORTRAN_ROOT="$lfortran_root"
        exec "$benchmark_pixi" run --locked --manifest-path "$lfortran_root/pixi.toml" -e llvm11 \
            /bin/bash "$entry_script" "$@"
    fi

    compiler="${LFORTRAN_COMPILER:-"$lfortran_root/build/llvm11/src/bin/lfortran"}"
    benchmark_fpm="$formal_root/benchmarks/.pixi/envs/default/bin/fpm"
    benchmark_python="$lfortran_root/.pixi/envs/llvm11/bin/python"
    benchmark_gfortran="$lfortran_root/.pixi/envs/llvm11/bin/gfortran"

    # LFortran's Metal runtime invokes clang through PATH, not CC alone.
    # Select the actual SDK tools after Pixi activation, without replacing
    # the explicitly selected Fortran, FPM or Python executables.
    if ! sdk_clang="$(/usr/bin/xcrun --sdk macosx --find clang)" ||
       ! sdk_clangxx="$(/usr/bin/xcrun --sdk macosx --find clang++)" ||
       ! sdkroot="$(/usr/bin/xcrun --sdk macosx --show-sdk-path)" ||
       [[ ! -x "$sdk_clang" || ! -d "$sdkroot" ]]; then
        echo "The macOS SDK is required; install Xcode Command Line Tools." >&2
        exit 2
    fi
    export PATH="$(dirname "$sdk_clang"):/usr/bin:/bin:$lfortran_root/build/llvm11/src/bin:$lfortran_root/.pixi/envs/llvm11/bin:$PATH"
    export CC="$sdk_clang" CXX="$sdk_clangxx" LFORTRAN_CC="$sdk_clang"
    export LFORTRAN_LINKER_PATH=/usr/bin SDKROOT="$sdkroot"
}

benchmark_require_fpm() {
    "$benchmark_pixi" install --locked --manifest-path "$formal_root/benchmarks/pixi.toml"
    if [[ ! -x "$benchmark_fpm" ]]; then
        echo "Managed FPM is missing; run pixi install --locked --manifest-path benchmarks/pixi.toml." >&2
        exit 2
    fi
}
