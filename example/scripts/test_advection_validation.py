from __future__ import annotations

import contextlib
import io
import math
from pathlib import Path
import shutil
import struct
import subprocess
import unittest
from unittest.mock import patch

import compare_2d_fields as csv_fields
import compare_2d_raw_fields as raw_fields
import validate_2d_advection_workspace as workspace


def snapshot(nx=13, ny=11, steps=3, dt="0.0001", mode="raw-final", bad_value=None):
    scalar = [0.5 + 0.01 * i + 0.02 * j for j in range(ny + 2) for i in range(nx + 2)]
    if mode == "raw-final":
        for j in range(1, ny + 1):
            for i in range(1, nx + 1):
                scalar[i + j * (nx + 2)] *= 0.99
    values = scalar
    if mode == "raw":
        values += [0.1] * ((nx + 1) * (ny + 2) + (nx + 2) * (ny + 1))
    if bad_value is not None:
        index, value = bad_value
        values[index] = value
    header = f"FADV2D1\n{nx} {ny} {steps} {dt}\nmode {mode}\n".encode()
    return header + struct.pack(f"<{len(values)}f", *values)


class WorkspaceValidationTests(unittest.TestCase):
    cases = 0

    @classmethod
    def setUpClass(cls):
        cls.scratch = Path(__file__).resolve().parents[2] / "build/formal-gpu/validate-workspace-selftest"
        cls.scratch.mkdir(parents=True, exist_ok=False)
        cls.addClassCleanup(shutil.rmtree, cls.scratch)

    def check_driver(self, *, initial=None, reference=None, candidate=None, native=None,
                     extra=(), error=None, launched=True, write_candidate=True):
        initial_path = self.scratch / "input.fadv2d"
        reference_path = self.scratch / "reference.fadv2d"
        initial_path.write_bytes(initial if initial is not None else snapshot(mode="raw"))
        reference_path.write_bytes(reference if reference is not None else snapshot())
        argv = ["validate", "--executable", str(self.scratch / "not-a-solver"),
                "--gpu-runner", "/usr/bin/env", "--workdir", str(self.scratch),
                "--raw-input", str(initial_path), "--reference-raw", str(reference_path)]
        if native is not None:
            native_path = self.scratch / "native.bin"
            native_path.write_bytes(native)
            argv += ["--native-final-raw", str(native_path)]

        def emit(command, cwd):
            if write_candidate:
                path = Path(command[command.index("--dump-final-raw") + 1])
                path.write_bytes(candidate if candidate is not None else snapshot())

        with patch.object(workspace, "run", side_effect=emit) as launch, \
                contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            if error is None:
                self.assertEqual(workspace.main(argv + list(extra)), 0)
            else:
                with self.assertRaisesRegex((SystemExit, OSError), error):
                    workspace.main(argv + list(extra))
        self.assertEqual(launch.call_count, int(launched))
        type(self).cases += 1

    def test_valid_metadata_and_fp32_rounding(self):
        self.check_driver(extra=("--evolution",))
        self.check_driver(extra=("--cells", "13,11", "--steps", "3", "--dt", "0.0001"))
        rounded = f"{raw_fields.fp32_timestep('0.0001', 'test'):.16e}"
        self.check_driver(reference=snapshot(dt=rounded), candidate=snapshot(dt=rounded),
                          extra=("--dt", rounded))
        self.check_driver(native=snapshot().split(b"\n", 3)[3])
        self.check_driver(extra=("--rtol", "0", "--atol", "0"))

    def test_wrong_candidate_and_reference_metadata(self):
        for field in ("reference", "candidate"):
            for key, value in (("nx", 14), ("ny", 12), ("steps", 999), ("dt", "0.123")):
                with self.subTest(field=field, key=key):
                    self.check_driver(**{field: snapshot(**{key: value})},
                                      error="configuration differs", launched=field == "candidate")

    def test_wrong_expected_metadata(self):
        for extra in (("--cells", "14,11"), ("--cells", "13,12"), ("--steps", "4")):
            self.check_driver(extra=extra, error="initial configuration mismatch", launched=False)
        self.check_driver(extra=("--dt", "0.0002"), error="initial timestep differs", launched=False)

    def test_nonfinite_or_negative_tolerances(self):
        for option in ("--rtol", "--atol"):
            for value in ("nan", "inf", "-inf", "-1"):
                with self.subTest(option=option, value=value):
                    self.check_driver(extra=(f"{option}={value}",),
                                      error="finite and non-negative", launched=False)
                    with self.assertRaisesRegex(SystemExit, "finite and non-negative"):
                        workspace.compare_fields([[1.0]], [[2.0]],
                                                 float(value) if option == "--rtol" else 0.0,
                                                 float(value) if option == "--atol" else 0.0)

    def test_invalid_initial_and_expected_timestep(self):
        for value in ("nan", "inf", "-inf", "0", "-1", "1e-100", "1e100", "invalid"):
            for location in ("initial", "expected"):
                with self.subTest(location=location, value=value):
                    options = ({"initial": snapshot(mode="raw", dt=value)} if location == "initial"
                               else {"extra": (f"--dt={value}",)})
                    self.check_driver(**options, error="timestep", launched=False)

    def test_invalid_configuration_options(self):
        for extra, error in (
            (("--cells", "0,11"), "cells >= 9"), (("--cells", "13,8"), "cells >= 9"),
            (("--cells", "13,11,9"), "cells >= 9"), (("--cells", "abc"), "must be nx"),
            (("--steps", "0"), "steps must be positive"), (("--steps", "-1"), "steps must be positive"),
        ):
            self.check_driver(extra=extra, error=error, launched=False)
        self.check_driver(initial=snapshot(mode="raw", steps=0),
                          error="positive step count", launched=False)

    def test_nonfinite_raw_fields(self):
        count = 15 * 13
        for value in (math.nan, math.inf, -math.inf):
            for field in ("initial", "reference", "candidate"):
                with self.subTest(value=value, field=field):
                    self.check_driver(**{field: snapshot(mode="raw" if field == "initial" else "raw-final",
                                                         bad_value=(16, value))},
                                      error="non-finite", launched=field == "candidate")
            for offset in (count, count + 14 * 13):
                self.check_driver(initial=snapshot(mode="raw", bad_value=(offset, value)),
                                  error="non-finite raw velocity", launched=False)
            payload = snapshot(bad_value=(16, value)).split(b"\n", 3)[3]
            self.check_driver(native=payload, error="non-finite", launched=False)

    def test_modes_and_payload_sizes(self):
        for field in ("initial", "reference", "candidate"):
            data = snapshot(mode="raw" if field == "initial" else "raw-final")
            wrong_mode = data.replace(b"mode raw\n", b"mode raw-final\n") if field == "initial" \
                else data.replace(b"mode raw-final\n", b"mode raw\n")
            self.check_driver(**{field: wrong_mode}, error="expected mode", launched=field == "candidate")
            for payload in (data[:-1], data + b"\0" * 4):
                self.check_driver(**{field: payload}, error="payload size", launched=field == "candidate")

    def test_invalid_headers(self):
        for data, error in (
            (snapshot(mode="raw").replace(b"FADV2D1", b"INVALID"), "invalid header"),
            (snapshot(mode="raw").replace(b"13 11 3", b"x 11 3"), "invalid dimensions"),
            (snapshot(mode="raw").replace(b"13 11 3", b"13 11"), "invalid dimensions"),
        ):
            self.check_driver(initial=data, error=error, launched=False)

    def test_field_mismatch_boundaries_and_evolution(self):
        self.check_driver(candidate=snapshot(bad_value=(16, 2.0)), error="field mismatch")
        self.check_driver(candidate=snapshot(bad_value=(0, 2.0)), error="fixed boundary changed")
        stationary = snapshot(mode="raw").split(b"\n", 3)[3][:15 * 13 * 4]
        final = b"FADV2D1\n13 11 3 0.0001\nmode raw-final\n" + stationary
        self.check_driver(reference=final, candidate=final)
        self.check_driver(candidate=final, extra=("--evolution",), error="field did not change")
        self.check_driver(reference=final, extra=("--evolution",),
                          error="field did not change", launched=False)

    def test_missing_output_cannot_reuse_stale_field(self):
        (self.scratch / "workspace.final.raw").write_bytes(snapshot())
        self.check_driver(write_candidate=False, error="No such file")

    def test_shared_field_comparator_rejects_invalid_arrays(self):
        for fields, error in (
            (([], []), "empty field"), (([[]], [[]]), "empty field"),
            (([[1.0], [1.0, 2.0]], [[1.0], [1.0]]), "shapes differ"),
            (([[math.nan]], [[1.0]]), "non-finite"),
            (([[1.0]], [[math.inf]]), "non-finite"),
        ):
            for evolution in (False, True):
                with self.assertRaisesRegex(SystemExit, error):
                    csv_fields.report_comparison(*fields, 1e-6, 1e-8, evolution)
        for value in (math.nan, math.inf, -1.0):
            with self.assertRaisesRegex(SystemExit, "finite and non-negative"):
                csv_fields.report_comparison([[1.0]], [[2.0]], value, 0.0, True)


def check_input_guards(executable: Path):
    executable = executable.resolve(strict=True)
    checks = 0

    def check(arguments, error=None):
        nonlocal checks
        result = subprocess.run([str(executable), *map(str, arguments)], capture_output=True,
                                text=True, timeout=5)
        output = result.stdout + result.stderr
        if error is None:
            passed = result.returncode == 0 and "input validation passed" in output
        else:
            passed = result.returncode != 0 and error in output
        if not passed:
            raise SystemExit(f"guard self-test {arguments}: exit {result.returncode}\n{output}")
        checks += 1
        print(f"PASS guard {arguments}: exit {result.returncode}; {error or 'accepted'}")

    def working_set(nx, ny):
        return 12 * ((nx + 2) * (ny + 2) + (nx + 1) * (ny + 2) + (nx + 2) * (ny + 1))

    cap = 512 * 1024 * 1024
    for nx, ny in ((1, 1), (5, 5), (9, 11), (51, 51), (257, 257), (2048, 2048)):
        check(("working-set", nx, ny))
    square = math.isqrt(cap // 36)
    while working_set(square, square) > cap:
        square -= 1
    for nx, ny in ((square - 1, square - 1), (square, square), (square + 1, square + 1), (4000, 4000)):
        check(("working-set", nx, ny), "exceeds 512 MiB" if working_set(nx, ny) > cap else None)
    for ny in (1, 5, 9, 127, 2048):
        limit = (cap // 12 - 5 * ny - 8) // (3 * ny + 5)
        for nx in (limit - 1, limit, limit + 1):
            error = "exceeds 512 MiB" if working_set(nx, ny) > cap else None
            check(("working-set", nx, ny), error)
            check(("working-set", ny, nx), error)
    for nx, ny, error in (
        (2**31 - 3, 2**31 - 3, "scalar buffers size overflow"),
        (800000000, 800000000, "working set size overflow"),
        (2**31 - 3, 9, "exceeds 512 MiB"), (9, 2**31 - 3, "exceeds 512 MiB"),
        (2**31 - 2, 9, "halo size overflow"), (9, 2**31 - 2, "halo size overflow"),
        (2**31 - 1, 9, "halo size overflow"), (9, 2**31 - 1, "halo size overflow"),
        (0, 9, "cells must be positive"), (9, 0, "cells must be positive"),
        (-1, 9, "cells must be positive"), (9, -2**31, "cells must be positive"),
    ):
        check(("working-set", nx, ny), error)
    for dt in ("nan", "inf", "+inf", "-inf", "0", "-0", "-1", "1e-100"):
        check(("timestep", dt), "dt must be finite and positive")
    for dt in ("1e-4", "1e-20", "1e-38", "1", "3.402823466e38"):
        check(("timestep", dt))
    check(("defaults",))
    print(f"self-test ok: {checks} allocation-free host guard checks; no solver or GPU calls")


def run_self_tests(guard_executable=None):
    WorkspaceValidationTests.cases = 0
    result = unittest.TextTestRunner(verbosity=2).run(
        unittest.defaultTestLoader.loadTestsFromTestCase(WorkspaceValidationTests))
    if not result.wasSuccessful():
        raise SystemExit("workspace validator self-tests failed")
    print(f"self-test ok: {WorkspaceValidationTests.cases} synthetic driver cases; no solver or GPU calls")
    if guard_executable is not None:
        check_input_guards(guard_executable)


if __name__ == "__main__":
    run_self_tests()
