#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

from compare_2d_fields import (
    compare_fields, load_and_validate, report_comparison, run_comparison as compare_csv,
    validate_tolerances,
)
from compare_2d_raw_fields import (
    check_final_field, check_initial_configuration, fp32_timestep,
    parse_col_major_payload, parse_raw_headered, parse_raw_headerless, read_matching_final,
)


def run(command, cwd: Path):
    subprocess.run(command, cwd=cwd, check=True)


def main(argv):
    parser = argparse.ArgumentParser(description="Validate the 2D advection-diffusion example")
    parser.add_argument("--executable")
    parser.add_argument("--gpu-runner", nargs="+", help="command prefix that owns the executable")
    parser.add_argument("--workdir", default="build/formal-gpu/workspace-validation")
    parser.add_argument("--cells", help="nx[,ny]; expected metadata with --raw-input")
    parser.add_argument("--steps", type=int, help="positive step count; expected metadata with --raw-input")
    parser.add_argument("--dt", help="positive FP32 timestep; expected metadata with --raw-input")
    parser.add_argument("--rtol", type=float, default=1e-6)
    parser.add_argument("--atol", type=float, default=1e-8)
    parser.add_argument("--raw-input")
    parser.add_argument("--reference-raw")
    parser.add_argument("--native-final-raw")
    parser.add_argument("--gpu-profile", action="store_true")
    parser.add_argument("--evolution", action="store_true", help="require evolution beyond comparison tolerances")
    parser.add_argument("--self-test", action="store_true", help="CPU-only validation helper/driver tests")
    parser.add_argument("--guard-executable", type=Path,
                        help="also self-test the allocation-free FPM validate-advection-inputs host example")
    args = parser.parse_args(argv[1:])

    validate_tolerances(args.rtol, args.atol)
    if args.self_test:
        from test_advection_validation import run_self_tests
        run_self_tests(args.guard_executable)
        return 0
    if args.guard_executable:
        parser.error("--guard-executable requires --self-test")
    if not args.executable or not args.gpu_runner:
        parser.error("--executable and --gpu-runner are required unless --self-test is used")
    if not args.raw_input and (args.reference_raw or args.native_final_raw):
        parser.error("--reference-raw and --native-final-raw require --raw-input")
    cells = None
    if args.cells is not None:
        try:
            cells = tuple(map(int, args.cells.split(",")))
        except ValueError as exc:
            raise SystemExit("--cells must be nx[,ny]") from exc
        if len(cells) == 1:
            cells = (cells[0], cells[0])
        if len(cells) != 2 or min(cells) < 9:
            raise SystemExit("--cells requires order-4 cells >= 9")
    if args.steps is not None and args.steps <= 0:
        raise SystemExit("--steps must be positive")
    if args.dt is not None:
        fp32_timestep(args.dt, "--dt")

    workdir = Path(args.workdir).resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    exe = Path(args.executable).resolve()
    repo_root = Path(__file__).resolve().parents[2]
    runner = list(args.gpu_runner)

    if args.raw_input:
        input_path = Path(args.raw_input).resolve()
        initial_snapshot = parse_raw_headered(input_path, expected_mode="raw")
        nx, ny, steps, _, initial = initial_snapshot
        check_initial_configuration(initial_snapshot, cells if cells is not None else (nx, ny),
                                    args.steps if args.steps is not None else steps, args.dt)
        references = []
        if args.reference_raw:
            references.append(("reference", read_matching_final(
                Path(args.reference_raw), initial_snapshot, "reference final")))
        if args.native_final_raw:
            references.append(("native", parse_raw_headerless(Path(args.native_final_raw), nx, ny)))
        for label, reference in references:
            check_final_field(initial, reference, label, args.rtol, args.atol, args.evolution)
        final_raw = workdir / "workspace.final.raw"
        final_raw.unlink(missing_ok=True)
        command = runner + [str(exe), "--solver", "workspace", "--input", str(input_path),
                            "--no-csv", "--dump-final-raw", str(final_raw)]
        if args.gpu_profile:
            command.append("--gpu-profile")
        run(command, repo_root)
        candidate = read_matching_final(final_raw, initial_snapshot, "workspace final")
        check_final_field(initial, candidate, "workspace", args.rtol, args.atol, args.evolution)
        for label, reference in references:
            print(f"{label} versus workspace:")
            report_comparison(reference, candidate, args.rtol, args.atol, False)
        return 0

    cells = cells if cells is not None else (9, 11)
    steps = args.steps if args.steps is not None else 3
    dt = args.dt if args.dt is not None else "1e-4"
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

    configuration = ["--cells", ",".join(map(str, cells)), "--steps", str(steps), "--dt", dt]
    operators_cmd = runner + [str(exe), "--solver", "operators", *configuration]
    workspace_cmd = runner + [str(exe), "--solver", "workspace", *configuration]
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
    if args.evolution:
        for label, initial_path, final_path in (
            ("operators", operators_initial, operators_final),
            ("workspace", workspace_initial, workspace_final),
        ):
            initial, final = load_and_validate(initial_path, final_path)
            check_final_field(initial, final, label, args.rtol, args.atol, True)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main(sys.argv))
    except (OSError, ValueError) as exc:
        raise SystemExit(str(exc)) from exc
