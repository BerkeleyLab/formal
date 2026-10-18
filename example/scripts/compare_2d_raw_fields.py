#!/usr/bin/env python3
from __future__ import annotations

import argparse
import math
import sys
from pathlib import Path

from compare_2d_fields import report_comparison
from validate_2d_advection_workspace import parse_raw_headered, parse_raw_headerless


def check_boundaries(initial, final, label):
    nx, ny = len(initial) - 2, len(initial[0]) - 2
    boundary = [(i, j) for i in range(nx + 2) for j in (0, ny + 1)]
    boundary += [(i, j) for i in (0, nx + 1) for j in range(1, ny + 1)]
    for i, j in boundary:
        if final[i][j] != initial[i][j]:
            raise SystemExit(f"{label}: fixed boundary changed at ({i},{j})")


def compare_raw_fields(initial_path, formal_path, native_path, cells, steps,
                       rtol=1e-6, atol=1e-8, evolution=False, cpu_path=None):
    if len(cells) != 2 or min(cells) < 9 or steps <= 0:
        raise SystemExit("expected order-4 cells >= 9 and a positive step count")
    if not all(math.isfinite(t) and t >= 0 for t in (rtol, atol)):
        raise SystemExit("--rtol and --atol must be finite and non-negative")
    initial_snapshot = parse_raw_headered(initial_path, expected_mode="raw")
    final_snapshot = parse_raw_headered(formal_path, expected_mode="raw-final")
    nx, ny, n, dt, initial = initial_snapshot
    if (nx, ny, n) != (*cells, steps):
        raise SystemExit("initial configuration mismatch")
    if final_snapshot[:4] != initial_snapshot[:4]:
        raise SystemExit("Formal final configuration differs from shared input")
    formal = final_snapshot[4]
    native = parse_raw_headerless(native_path, nx, ny)
    finals = [("Formal", formal), ("native", native)]
    if cpu_path is not None:
        finals.append(("CPU oracle", parse_raw_headerless(cpu_path, nx, ny)))

    initial_scale = max(abs(value) for row in initial for value in row)
    if evolution and initial_scale == 0:
        raise SystemExit("--evolution requires a nonzero initial field")
    print(f"cells={nx},{ny} steps={n} dt={dt:.16e} elements={(nx + 2) * (ny + 2)}")
    for label, final in finals:
        if evolution and not any(value != 0 for row in final for value in row):
            raise SystemExit(f"{label}: zero final field from nonzero input")
        check_boundaries(initial, final, label)
        if evolution:
            print(f"{label} evolution:")
            max_abs, _, relative_l2, _ = report_comparison(initial, final, rtol, atol, True)
            if max_abs <= atol + rtol * initial_scale or relative_l2 <= rtol:
                raise SystemExit(f"{label}: evolution does not exceed comparison tolerances")
    print("Formal versus native:")
    report_comparison(formal, native, rtol, atol, False)
    if cpu_path is not None:
        for label, final in finals[:2]:
            print(f"CPU oracle versus {label}:")
            report_comparison(finals[2][1], final, rtol, atol, False)
    print("full-field comparison passed; fixed boundaries preserved")


def self_test():
    import contextlib
    import io
    import shutil
    import struct

    scratch = Path(__file__).resolve().parents[2] / "build/formal-gpu/compare_2d_raw_fields-selftest"
    scratch.mkdir(parents=True, exist_ok=False)
    nx, ny, steps = 13, 11, 3
    count = (nx + 2) * (ny + 2)
    scalar = [0.5 + math.exp(-((i - 5) ** 2 + (j - 4) ** 2) / 8)
              for j in range(ny + 2) for i in range(nx + 2)]
    evolved = scalar.copy()
    for j in range(1, ny + 1):
        for i in range(1, nx + 1):
            evolved[i + j * (nx + 2)] *= 0.99
    velocities = [0.1] * ((nx + 1) * (ny + 2) + (nx + 2) * (ny + 1))
    input_header = f"FADV2D1\n{nx} {ny} {steps} 0.0001\nmode raw\n".encode()
    final_header = input_header.replace(b"mode raw\n", b"mode raw-final\n")

    def pack(values):
        return struct.pack(f"<{len(values)}f", *values)

    initial_bytes = input_header + pack(scalar + velocities)
    formal_bytes = final_header + pack(evolved)
    native_bytes = pack(evolved)
    paths = [scratch / name for name in ("input.fadv2d", "formal.fadv2d", "native.bin")]
    checks = 0

    def check(label, replacements=None, error=None, **kwargs):
        nonlocal checks
        data = [initial_bytes, formal_bytes, native_bytes]
        for index, payload in (replacements or {}).items():
            data[index] = payload
        for path, payload in zip(paths, data):
            if payload is None:
                path.unlink(missing_ok=True)
            else:
                path.write_bytes(payload)
        options = dict(cells=(nx, ny), steps=steps, evolution=True)
        options.update(kwargs)
        try:
            with contextlib.redirect_stdout(io.StringIO()):
                compare_raw_fields(*paths, **options)
        except (SystemExit, OSError, ValueError) as exc:
            if error is None or error not in str(exc):
                raise SystemExit(f"self-test {label}: unexpected failure: {exc}") from exc
        else:
            if error is not None:
                raise SystemExit(f"self-test {label}: expected rejection")
        checks += 1
        print(f"PASS {label}")

    try:
        check("synthetic evolved pair")
        check("unchanged initial", {1: final_header + pack(scalar), 2: pack(scalar)},
              "field did not change")
        zeros = pack([0.0] * count)
        check("both finals zero", {1: final_header + zeros, 2: zeros}, "zero final field")
        check("Formal unchanged", {1: final_header + pack(scalar)}, "field did not change")
        check("native unchanged", {2: pack(scalar)}, "field did not change")
        tiny = scalar.copy()
        tiny[nx + 3] += 1e-7
        check("roundoff is not evolution", {1: final_header + pack(tiny), 2: pack(tiny)},
              "evolution does not exceed")
        zero_input = input_header + zeros + pack(velocities)
        check("zero initial cannot require evolution", {0: zero_input}, "nonzero initial")
        check("stationary zero remains valid without evolution",
              {0: zero_input, 1: final_header + zeros, 2: zeros}, evolution=False)
        drift = evolved.copy()
        drift[0] += 0.25
        check("both boundaries drift", {1: final_header + pack(drift), 2: pack(drift)},
              "fixed boundary changed")
        check("native boundary drift", {2: pack(drift)}, "native: fixed boundary changed")
        wrong = evolved.copy()
        wrong[nx + 3] += 0.05
        check("finite interior mismatch", {2: pack(wrong)}, "field mismatch")
        for index, name in enumerate(("initial", "Formal", "native")):
            check(f"missing {name}", {index: None}, "No such file")
            check(f"truncated {name}", {index: (initial_bytes, formal_bytes, native_bytes)[index][:-4]},
                  "payload size")
            check(f"extra {name}", {index: (initial_bytes, formal_bytes, native_bytes)[index] + b"\0" * 4},
                  "payload size")
        check("partial FP32 element", {2: native_bytes[:-1]}, "payload size")
        for value in (math.nan, math.inf):
            for index, name in enumerate(("initial scalar", "Formal scalar", "native scalar", "x velocity", "y velocity")):
                if index in (0, 3, 4):
                    values = scalar + velocities
                    offset = 0 if index == 0 else count
                    if index == 4:
                        offset += (nx + 1) * (ny + 2)
                    values[offset] = value
                    change = {0: input_header + pack(values)}
                else:
                    values = evolved.copy()
                    values[nx + 3] = value
                    change = {index: (final_header if index == 1 else b"") + pack(values)}
                check(f"{name} {value}", change, "non-finite")
        check("expected cells", error="initial configuration mismatch", cells=(ny, nx))
        check("expected steps", error="initial configuration mismatch", steps=4)
        check("zero expected steps", error="positive step count", steps=0)
        check("wrong Formal steps", {1: formal_bytes.replace(b"13 11 3", b"13 11 4")},
              "configuration differs")
        check("wrong Formal timestep", {1: formal_bytes.replace(b"0.0001", b"0.0002")},
              "configuration differs")
        for dt in (b"nan", b"inf", b"0", b"-1", b"1e-100"):
            check(f"invalid initial dt {dt!r}", {0: initial_bytes.replace(b"0.0001", dt)},
                  "timestep must be finite and positive")
        check("overflowing initial timestep", {0: initial_bytes.replace(b"0.0001", b"1e100")},
              "invalid FP32 timestep")
        rounded_dt = f"{struct.unpack('<f', struct.pack('<f', 0.0001))[0]:.16e}".encode()
        check("equivalent FP32 timestep", {1: formal_bytes.replace(b"0.0001", rounded_dt)})
        check("wrong input mode", {0: initial_bytes.replace(b"mode raw\n", b"mode raw-final\n")},
              "expected mode raw,")
        check("wrong final mode", {1: formal_bytes.replace(b"raw-final", b"raw")},
              "expected mode raw-final")
        check("bad initial header", {0: initial_bytes.replace(b"FADV2D1", b"INVALID")},
              "invalid header")
        check("invalid tolerance", error="finite and non-negative", rtol=math.nan)
        cpu_path = scratch / "cpu.bin"
        cpu_path.write_bytes(native_bytes)
        check("synthetic CPU payload agreement", cpu_path=cpu_path)
        for name, values, error in (
            ("CPU unchanged", scalar, "field did not change"),
            ("CPU zero", [0.0] * count, "zero final field"),
            ("CPU boundary drift", drift, "fixed boundary changed"),
            ("CPU interior mismatch", wrong, "field mismatch"),
            ("CPU nonfinite", [math.nan] * count, "non-finite"),
        ):
            cpu_path.write_bytes(pack(values))
            check(name, error=error, cpu_path=cpu_path)
        cpu_path.unlink()
        check("missing CPU field", error="No such file", cpu_path=cpu_path)
        print(f"self-test ok: {checks} CPU-only comparator checks; synthetic fields, not solver validation")
    finally:
        shutil.rmtree(scratch)


def main(argv):
    parser = argparse.ArgumentParser(description="Compare shared-input Formal/native FP32 raw fields")
    parser.add_argument("initial", nargs="?")
    parser.add_argument("formal_final", nargs="?")
    parser.add_argument("native_final", nargs="?")
    parser.add_argument("--cells", help="expected nx,ny")
    parser.add_argument("--steps", type=int, help="expected positive step count")
    parser.add_argument("--rtol", type=float, default=1e-6)
    parser.add_argument("--atol", type=float, default=1e-8)
    parser.add_argument("--evolution", action="store_true",
                        help="require nonzero fields and evolution beyond the comparison tolerances")
    parser.add_argument("--cpu-final", type=Path, help="also compare a native CPU oracle payload")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv[1:])
    if args.self_test:
        self_test()
        return 0
    if any(value is None for value in (args.initial, args.formal_final, args.native_final, args.cells, args.steps)):
        parser.error("initial, formal_final, native_final, --cells and --steps are required")
    try:
        cells = tuple(map(int, args.cells.split(",")))
        compare_raw_fields(Path(args.initial), Path(args.formal_final), Path(args.native_final),
                           cells, args.steps, args.rtol, args.atol, args.evolution, args.cpu_final)
    except (OSError, ValueError) as exc:
        raise SystemExit(str(exc)) from exc
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
