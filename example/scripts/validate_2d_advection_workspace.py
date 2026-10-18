#!/usr/bin/env python3
from __future__ import annotations

import argparse
import math
import struct
import subprocess
import sys
from pathlib import Path

from compare_2d_fields import run_comparison as compare_csv


def parse_col_major_payload(payload: bytes, nx: int, ny: int):
    if nx < 1 or ny < 1:
        raise SystemExit("raw field dimensions must be positive")
    count = (nx + 2) * (ny + 2)
    if len(payload) != count * 4:
        raise SystemExit(f"unexpected raw payload size: expected {count * 4} bytes")
    values = struct.unpack(f"<{count}f", payload)
    if not all(math.isfinite(value) for value in values):
        raise SystemExit("non-finite raw scalar field")
    return [[values[i + j * (nx + 2)] for j in range(ny + 2)] for i in range(nx + 2)]


def parse_raw_headered(path: Path, *, expected_mode=None):
    with path.open("rb") as handle:
        header = handle.readline().decode("ascii").strip()
        if header != "FADV2D1":
            raise SystemExit(f"{path}: invalid header")
        dims = handle.readline().decode("ascii").strip().split()
        if len(dims) != 4:
            raise SystemExit(f"{path}: invalid dimensions line")
        nx, ny, steps = map(int, dims[:3])
        try:
            dt = struct.unpack("<f", struct.pack("<f", float(dims[3])))[0]
        except (ValueError, OverflowError) as exc:
            raise SystemExit(f"{path}: invalid FP32 timestep") from exc
        if nx < 1 or ny < 1 or steps < 0:
            raise SystemExit(f"{path}: invalid grid or step count")
        if not math.isfinite(dt) or dt <= 0:
            raise SystemExit(f"{path}: timestep must be finite and positive")
        mode = handle.readline().decode("ascii").strip()
        if mode not in {"mode raw-final", "mode raw"}:
            raise SystemExit(f"{path}: invalid mode {mode!r}")
        if expected_mode is not None and mode != f"mode {expected_mode}":
            raise SystemExit(f"{path}: expected mode {expected_mode}, got {mode!r}")
        payload = handle.read()
    if mode == "mode raw":
        scalar_bytes = (nx + 2) * (ny + 2) * 4
        velocity_bytes = ((nx + 1) * (ny + 2) + (nx + 2) * (ny + 1)) * 4
        if len(payload) != scalar_bytes + velocity_bytes:
            raise SystemExit(f"{path}: unexpected raw input payload size")
        if not all(math.isfinite(value) for value, in struct.iter_unpack("<f", payload[scalar_bytes:])):
            raise SystemExit(f"{path}: non-finite raw velocity field")
        payload = payload[:scalar_bytes]
    return nx, ny, steps, dt, parse_col_major_payload(payload, nx, ny)


def parse_raw_headerless(path: Path, nx: int, ny: int):
    payload = path.read_bytes()
    return parse_col_major_payload(payload, nx, ny)


def compare_fields(reference, observed, rtol: float, atol: float):
    if len(reference) != len(observed) or len(reference[0]) != len(observed[0]):
        raise SystemExit("raw field shapes differ")
    max_abs = 0.0
    max_rel = 0.0
    changed = False
    for i in range(len(reference)):
        for j in range(len(reference[0])):
            ref = reference[i][j]
            obs = observed[i][j]
            diff = abs(obs - ref)
            scale = max(abs(ref), abs(obs))
            max_abs = max(max_abs, diff)
            max_rel = max(max_rel, diff / scale if scale else 0.0)
            changed = changed or diff != 0.0
            if diff > atol + rtol * scale:
                raise SystemExit(
                    f"raw field mismatch at ({i},{j}): abs diff {diff:.16e} exceeds "
                    f"atol+rtol*scale = {(atol + rtol * scale):.16e}"
                )
    print(f"raw_max_abs_diff={max_abs:.16e}")
    print(f"raw_max_rel_diff={max_rel:.16e}")
    print(f"raw_changed={str(changed).lower()}")


def run(command, cwd: Path):
    subprocess.run(command, cwd=cwd, check=True)


def main(argv):
    parser = argparse.ArgumentParser(description="Validate the 2D advection-diffusion example")
    parser.add_argument("--executable", required=True)
    parser.add_argument("--gpu-runner", nargs="+", required=True, help="command prefix that owns the executable")
    parser.add_argument("--workdir", default="build/formal-gpu/workspace-validation")
    parser.add_argument("--cells", default="9,11")
    parser.add_argument("--steps", default="3")
    parser.add_argument("--dt", default="1e-4")
    parser.add_argument("--rtol", type=float, default=1e-6)
    parser.add_argument("--atol", type=float, default=1e-8)
    parser.add_argument("--raw-input")
    parser.add_argument("--reference-raw")
    parser.add_argument("--native-final-raw")
    parser.add_argument("--gpu-profile", action="store_true")
    args = parser.parse_args(argv[1:])

    if args.rtol < 0.0 or args.atol < 0.0:
        raise SystemExit("--rtol and --atol must be non-negative")

    workdir = Path(args.workdir)
    workdir.mkdir(parents=True, exist_ok=True)
    exe = Path(args.executable).resolve()
    repo_root = Path(__file__).resolve().parents[2]
    runner = list(args.gpu_runner)

    if args.raw_input:
        final_raw = workdir / "workspace.final.raw"
        command = runner + [str(exe), "--solver", "workspace", "--input", args.raw_input, "--no-csv", "--dump-final-raw", str(final_raw)]
        if args.gpu_profile:
            command.append("--gpu-profile")
        run(command, repo_root)
        candidate_nx, candidate_ny, candidate_steps, candidate_dt, candidate = parse_raw_headered(final_raw)
        if args.native_final_raw:
            native = parse_raw_headerless(Path(args.native_final_raw), candidate_nx, candidate_ny)
            compare_fields(candidate, native, args.rtol, args.atol)
        if args.reference_raw:
            _, _, _, _, reference = parse_raw_headered(Path(args.reference_raw))
            compare_fields(reference, candidate, args.rtol, args.atol)
        return 0

    operators_prefix = workdir / "operators"
    workspace_prefix = workdir / "workspace"
    operators_final = operators_prefix.with_suffix(".scalar-final.csv")
    workspace_final = workspace_prefix.with_suffix(".scalar-final.csv")
    operators_initial = operators_prefix.with_suffix(".scalar-initial.csv")
    workspace_initial = workspace_prefix.with_suffix(".scalar-initial.csv")

    for prefix in (operators_prefix, workspace_prefix):
        for suffix in ("scalar-initial.csv", "scalar-final.csv", "velocity.csv"):
            p = prefix.parent / f"{prefix.name}.{suffix}"
            if p.exists():
                p.unlink()

    operators_cmd = runner + [str(exe), "--solver", "operators", "--cells", args.cells, "--steps", args.steps, "--dt", args.dt]
    workspace_cmd = runner + [str(exe), "--solver", "workspace", "--cells", args.cells, "--steps", args.steps, "--dt", args.dt]
    if args.gpu_profile:
        operators_cmd.append("--gpu-profile")
        workspace_cmd.append("--gpu-profile")
    run(operators_cmd, repo_root)
    (workdir / "operators.scalar-initial.csv").write_bytes((repo_root / "example/scripts/scalar-initial.csv").read_bytes())
    (workdir / "operators.scalar-final.csv").write_bytes((repo_root / "example/scripts/scalar-final.csv").read_bytes())
    run(workspace_cmd, repo_root)
    (workdir / "workspace.scalar-initial.csv").write_bytes((repo_root / "example/scripts/scalar-initial.csv").read_bytes())
    (workdir / "workspace.scalar-final.csv").write_bytes((repo_root / "example/scripts/scalar-final.csv").read_bytes())
    compare_csv(operators_final, workspace_final, rtol=args.rtol, atol=args.atol, evolution=False)
    compare_csv(operators_initial, workspace_initial, rtol=args.rtol, atol=args.atol, evolution=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
