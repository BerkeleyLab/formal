#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import math
import shutil
import sys
from pathlib import Path


def finite_float(raw: str, path: Path, line_number: int, kind: str) -> float:
    try:
        value = float(raw)
    except ValueError as exc:
        raise SystemExit(f"{path}:{line_number}: invalid {kind}: {raw!r}") from exc
    if not math.isfinite(value):
        raise SystemExit(f"{path}:{line_number}: non-finite {kind}")
    return value


def load_field(path: Path):
    if not path.exists():
        raise SystemExit(f"{path}: missing file")

    xs = []
    ys = []
    x_seen = set()
    y_seen = set()
    values = {}
    with path.open(newline="") as handle:
        reader = csv.reader(handle)
        header = next(reader, None)
        if header is None:
            raise SystemExit(f"{path}: empty file")
        if len(header) < 3:
            raise SystemExit(f"{path}: expected x, y, value columns")
        for line_number, row in enumerate(reader, start=2):
            if not row:
                continue
            if len(row) != 3:
                raise SystemExit(f"{path}:{line_number}: expected exactly 3 CSV columns")
            x = finite_float(row[0], path, line_number, "x")
            y = finite_float(row[1], path, line_number, "y")
            value = finite_float(row[2], path, line_number, "value")
            if x not in x_seen:
                xs.append(x)
                x_seen.add(x)
            if y not in y_seen:
                ys.append(y)
                y_seen.add(y)
            key = (x, y)
            if key in values:
                raise SystemExit(f"{path}:{line_number}: duplicate grid point {x},{y}")
            values[key] = value

    if not xs or not ys:
        raise SystemExit(f"{path}: empty field data")
    xs.sort()
    ys.sort()
    if len(values) != len(xs) * len(ys):
        raise SystemExit(f"{path}: incomplete field data")

    field = [[values[(x, y)] for y in ys] for x in xs]
    return xs, ys, field


def compare_fields(reference, observed, rtol: float, atol: float):
    if len(reference) != len(observed) or len(reference[0]) != len(observed[0]):
        raise SystemExit("field shapes differ")

    max_abs_diff = 0.0
    max_rel_diff = 0.0
    changed = False
    for i in range(len(reference)):
        for j in range(len(reference[0])):
            ref = reference[i][j]
            obs = observed[i][j]
            diff = abs(obs - ref)
            scale = max(abs(ref), abs(obs))
            max_abs_diff = max(max_abs_diff, diff)
            max_rel_diff = max(max_rel_diff, diff / scale if scale else 0.0)
            changed = changed or diff != 0.0
            if diff > (atol + rtol * scale):
                raise SystemExit(
                    f"field mismatch at ({i},{j}): abs diff {diff:.16e} exceeds "
                    f"atol+rtol*scale = {(atol + rtol * scale):.16e}"
                )
    ref_sq = sum(reference[i][j] * reference[i][j] for i in range(len(reference)) for j in range(len(reference[0])))
    diff_sq = sum((observed[i][j] - reference[i][j]) ** 2 for i in range(len(reference)) for j in range(len(reference[0])))
    relative_l2 = math.sqrt(diff_sq / ref_sq) if ref_sq else (0.0 if diff_sq == 0.0 else math.inf)
    return max_abs_diff, max_rel_diff, relative_l2, changed


def summarize_field_differences(reference, observed):
    if len(reference) != len(observed) or len(reference[0]) != len(observed[0]):
        raise SystemExit("field shapes differ")

    max_abs_diff = 0.0
    max_rel_diff = 0.0
    changed = False
    for i in range(len(reference)):
        for j in range(len(reference[0])):
            ref = reference[i][j]
            obs = observed[i][j]
            diff = abs(obs - ref)
            scale = max(abs(ref), abs(obs))
            max_abs_diff = max(max_abs_diff, diff)
            max_rel_diff = max(max_rel_diff, diff / scale if scale else 0.0)
            changed = changed or diff != 0.0
    ref_sq = sum(reference[i][j] * reference[i][j] for i in range(len(reference)) for j in range(len(reference[0])))
    diff_sq = sum((observed[i][j] - reference[i][j]) ** 2 for i in range(len(reference)) for j in range(len(reference[0])))
    relative_l2 = math.sqrt(diff_sq / ref_sq) if ref_sq else (0.0 if diff_sq == 0.0 else math.inf)
    return max_abs_diff, max_rel_diff, relative_l2, changed


def load_and_validate(reference_path: Path, observed_path: Path):
    reference_x, reference_y, reference = load_field(reference_path)
    observed_x, observed_y, observed = load_field(observed_path)
    if reference_x != observed_x or reference_y != observed_y:
        raise SystemExit("coordinate grids differ")
    return reference, observed


def run_comparison(reference_path: Path, observed_path: Path, rtol: float, atol: float, evolution: bool):
    reference, observed = load_and_validate(reference_path, observed_path)
    if evolution:
        max_abs_diff, max_rel_diff, relative_l2, changed = summarize_field_differences(reference, observed)
    else:
        max_abs_diff, max_rel_diff, relative_l2, changed = compare_fields(reference, observed, rtol, atol)
    print(f"max_abs_diff={max_abs_diff:.16e}")
    print(f"max_rel_diff={max_rel_diff:.16e}")
    print(f"relative_l2={relative_l2:.16e}")
    print(f"changed={str(changed).lower()}")
    if evolution and not changed:
        raise SystemExit("field did not change")


def write_csv(path: Path, xs, ys, func):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(["x", "y", "value"])
        for x in xs:
            for y in ys:
                writer.writerow([f"{x:.16e}", f"{y:.16e}", f"{func(x, y):.16e}"])


def expect_failure(label: str, func):
    try:
        func()
    except SystemExit:
        return
    raise SystemExit(f"self-test failed: expected failure for {label}")


def self_test():
    repo_root = Path(__file__).resolve().parents[2]
    scratch = repo_root / "build" / "formal-gpu" / "compare_2d_fields-selftest"
    if scratch.exists():
        shutil.rmtree(scratch)
    scratch.mkdir(parents=True)

    xs = [0.0, 1.0]
    ys = [0.0, 2.0]
    reference = scratch / "reference.csv"
    candidate = scratch / "candidate.csv"
    shifted = scratch / "shifted.csv"
    shifted_grid = scratch / "shifted-grid.csv"
    malformed = scratch / "malformed.csv"
    nonfinite = scratch / "nonfinite.csv"
    duplicate = scratch / "duplicate.csv"
    empty = scratch / "empty.csv"

    write_csv(reference, xs, ys, lambda x, y: 0.0)
    write_csv(candidate, xs, ys, lambda x, y: 0.0)
    write_csv(shifted, xs, ys, lambda x, y: 1.0 if x == 1.0 and y == 2.0 else 0.0)
    write_csv(shifted_grid, [0.0, 1.1], ys, lambda x, y: 0.0)

    malformed.write_text("x,y,value\n0,0\n", encoding="utf-8")
    nonfinite.write_text("x,y,value\n0,0,nan\n", encoding="utf-8")
    duplicate.write_text("x,y,value\n0,0,1\n0,0,2\n", encoding="utf-8")
    empty.write_text("", encoding="utf-8")

    run_comparison(reference, candidate, 0.0, 0.0, False)
    run_comparison(reference, shifted, 0.0, 0.0, True)
    expect_failure("mismatched values", lambda: run_comparison(reference, shifted, 0.0, 0.0, False))
    expect_failure("mismatched grid", lambda: run_comparison(reference, shifted_grid, 0.0, 0.0, False))
    expect_failure("malformed csv", lambda: load_field(malformed))
    expect_failure("nonfinite csv", lambda: load_field(nonfinite))
    expect_failure("duplicate csv", lambda: load_field(duplicate))
    expect_failure("empty csv", lambda: load_field(empty))
    print("self-test ok")


def main(argv):
    parser = argparse.ArgumentParser(description="Compare 2D field CSVs")
    parser.add_argument("reference", nargs="?")
    parser.add_argument("candidate", nargs="?")
    parser.add_argument("--rtol", type=float, default=0.0)
    parser.add_argument("--atol", type=float, default=0.0)
    parser.add_argument("--evolution", action="store_true", help="require candidate to differ from reference")
    parser.add_argument("--self-test", action="store_true", help="run comparator self-tests")
    args = parser.parse_args(argv[1:])

    if args.self_test:
        self_test()
        return 0

    if args.reference is None or args.candidate is None:
        parser.error("reference and candidate CSV paths are required unless --self-test is used")

    if not math.isfinite(args.rtol) or not math.isfinite(args.atol) or args.rtol < 0.0 or args.atol < 0.0:
        raise SystemExit("--rtol and --atol must be finite and non-negative")

    run_comparison(Path(args.reference), Path(args.candidate), args.rtol, args.atol, args.evolution)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
