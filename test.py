#!/usr/bin/env python3
#
# Viper test runner
#
# Put directives at the top of a .vp test file. Each directive starts with //!.
#
# Stages:
#   //! stage: skip      skip this test (prints yellow "skip")
#   //! stage: check     run vxc check only
#   //! stage: compile   compile to an object file
#   //! stage: run       compile, link, and execute
#
# Compile result:
#   //! expect-compile: ok      default
#   //! expect-compile: fail    compiler must fail
#   //! expect-error: "text"    required with expect-compile: fail
#
# Run-only expectations:
#   //! expect-exit: 0
#   //! expect-stdout: "exact output\n"
#   //! expect-stdout-contains: "partial output"
#   //! expect-stderr: "exact error\n"
#   //! expect-stderr-contains: "partial error"
#
# Examples:
#   //! stage: skip
#
#   //! stage: check
#   //! expect-compile: fail
#   //! expect-error: "type mismatch"
#
#   //! stage: run
#   //! expect-exit: 0
#   //! expect-stdout: "hello\n"

import argparse
import ast
import concurrent.futures
import os
from pathlib import Path
import shutil
import subprocess
import sys
import uuid


REPO = Path(__file__).resolve().parent
TEST_DIR = REPO / "test"
TEST_TMP = TEST_DIR / "tmp"
SEGFAULT_CODES = {-11, 139, 0xC0000005}
GREEN = "\033[32m"
YELLOW = "\033[33m"
RED = "\033[31m"
RESET = "\033[0m"


class TestError(Exception):
    pass


def parse_value(value):
    value = value.strip()
    if len(value) >= 2 and value[0] == '"' and value[-1] == '"':
        try:
            return ast.literal_eval(value)
        except (SyntaxError, ValueError) as e:
            raise TestError(f"invalid quoted directive value {value!r}: {e}") from e
    return value


def parse_directives(path):
    directives = {}
    with path.open("r", encoding="utf-8") as f:
        for line in f:
            stripped = line.strip()
            if not stripped.startswith("//!"):
                continue
            body = stripped[3:].strip()
            if ":" not in body:
                raise TestError(f"invalid directive {stripped!r}")
            key, value = body.split(":", 1)
            key = key.strip()
            if key in directives:
                raise TestError(f"duplicate directive {key!r}")
            directives[key] = parse_value(value)

    stage = directives.get("stage")
    if stage not in {"skip", "check", "compile", "run"}:
        raise TestError("missing or invalid directive: //! stage: skip | check | compile | run")

    expect_compile = directives.get("expect-compile", "ok")
    if expect_compile not in {"ok", "fail"}:
        raise TestError("expect-compile must be 'ok' or 'fail'")

    if expect_compile == "fail" and "expect-error" not in directives:
        raise TestError("expect-compile: fail requires expect-error")

    if stage != "run":
        for key in ("expect-exit", "expect-stdout", "expect-stdout-contains",
                    "expect-stderr", "expect-stderr-contains"):
            if key in directives:
                raise TestError(f"{key} is only valid with stage: run")

    if "expect-exit" in directives:
        try:
            directives["expect-exit"] = int(directives["expect-exit"])
        except ValueError as e:
            raise TestError("expect-exit must be an integer") from e

    return directives


def run_cmd(args, cwd, timeout):
    return subprocess.run(
        args,
        cwd=cwd,
        text=True,
        capture_output=True,
        timeout=timeout,
    )


def native_path(path):
    text = str(path)
    if len(text) >= 3 and text[1] == ":" and text[2] == "/":
        return text.replace("/", "\\")
    return text


def make_run_dir():
    TEST_TMP.mkdir(exist_ok=True)
    while True:
        path = TEST_TMP / f"run-{uuid.uuid4().hex}"
        try:
            path.mkdir()
            return path
        except FileExistsError:
            continue


def find_std_objects(vxc):
    vxc_path = shutil.which(vxc) if not os.path.isabs(vxc) else vxc
    if not vxc_path:
        raise TestError(f"cannot find compiler {vxc!r}")

    std_dir = Path(vxc_path).resolve().parent / "std"
    objs = sorted(std_dir.glob("*.obj"))
    if not objs:
        raise TestError(f"no std object files found in {std_dir}")
    return [native_path(obj) for obj in objs]


def check_compile_result(path, directives, result):
    expect_compile = directives.get("expect-compile", "ok")

    if result.returncode in SEGFAULT_CODES:
        raise TestError(f"compiler crashed with exit code {result.returncode}")

    if expect_compile == "ok":
        if result.returncode != 0:
            raise TestError(
                f"compiler failed with exit code {result.returncode}\n"
                f"stderr:\n{result.stderr}"
            )
        return

    if result.returncode == 0:
        raise TestError("compiler succeeded, expected failure")

    expected_error = directives["expect-error"]
    if expected_error not in result.stderr:
        raise TestError(
            f"compiler stderr did not contain {expected_error!r}\n"
            f"stderr:\n{result.stderr}"
        )


def check_output(directives, result):
    expected_exit = directives.get("expect-exit", 0)
    if result.returncode != expected_exit:
        raise TestError(
            f"program exited with {result.returncode}, expected {expected_exit}\n"
            f"stdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}"
        )

    exact_stdout = directives.get("expect-stdout")
    if exact_stdout is not None and result.stdout != exact_stdout:
        raise TestError(f"stdout mismatch\nexpected: {exact_stdout!r}\nactual:   {result.stdout!r}")

    stdout_contains = directives.get("expect-stdout-contains")
    if stdout_contains is not None and stdout_contains not in result.stdout:
        raise TestError(f"stdout did not contain {stdout_contains!r}\nstdout:\n{result.stdout}")

    exact_stderr = directives.get("expect-stderr")
    if exact_stderr is not None and result.stderr != exact_stderr:
        raise TestError(f"stderr mismatch\nexpected: {exact_stderr!r}\nactual:   {result.stderr!r}")

    stderr_contains = directives.get("expect-stderr-contains")
    if stderr_contains is not None and stderr_contains not in result.stderr:
        raise TestError(f"stderr did not contain {stderr_contains!r}\nstderr:\n{result.stderr}")


def run_test(path, args):
    path = Path(path).resolve()
    name = str(path.relative_to(REPO))

    try:
        directives = parse_directives(path)
        if directives["stage"] == "skip":
            return name, "skip", ""

        temp_dir = make_run_dir()
        try:
            obj = temp_dir / f"{path.stem}.obj"
            exe = temp_dir / f"{path.stem}.exe"

            if directives["stage"] == "check":
                result = run_cmd([args.vxc, "check", native_path(path)], REPO, args.timeout)
                check_compile_result(path, directives, result)
                return name, "ok", ""

            compile_result = run_cmd([args.vxc, "comp", "-o", native_path(obj), native_path(path)], REPO, args.timeout)
            check_compile_result(path, directives, compile_result)
            if directives.get("expect-compile", "ok") == "fail":
                return name, "ok", ""

            if directives["stage"] == "compile":
                if not obj.exists():
                    raise TestError(f"compiler did not produce {obj}")
                return name, "ok", ""

            link_cmd = [args.linker, native_path(obj), *args.std_objs, "-o", native_path(exe), "-e", "main", *args.ldflags]
            link_result = run_cmd(link_cmd, REPO, args.timeout)
            if link_result.returncode != 0:
                raise TestError(
                    f"linker failed with exit code {link_result.returncode}\n"
                    f"stderr:\n{link_result.stderr}"
                )

            run_result = run_cmd([native_path(exe)], temp_dir, args.timeout)
            check_output(directives, run_result)
            return name, "ok", ""
        finally:
            shutil.rmtree(temp_dir, ignore_errors=True)
    except subprocess.TimeoutExpired as e:
        return name, "fail", f"timed out after {e.timeout}s"
    except TestError as e:
        return name, "fail", str(e)


def collect_tests(paths):
    tests = []
    for raw in paths:
        path = Path(raw)
        if path.is_dir():
            tests.extend(sorted(p for p in path.rglob("*.vp") if TEST_TMP not in p.parents))
        else:
            tests.append(path)
    return tests


def color(text, code, enabled):
    if not enabled:
        return text
    return f"{code}{text}{RESET}"


def main(argv):
    parser = argparse.ArgumentParser(description="Run Viper directive tests.")
    parser.add_argument("paths", nargs="*", default=[str(TEST_DIR)], help="test files or directories")
    parser.add_argument("-j", "--jobs", type=int, default=os.cpu_count() or 1, help="parallel jobs")
    parser.add_argument("--vxc", default="vxc", help="Viper compiler command")
    parser.add_argument("--linker", default="ld", help="linker command")
    parser.add_argument("--timeout", type=float, default=10.0, help="per-command timeout in seconds")
    parser.add_argument("--color", choices=("auto", "always", "never"), default="auto", help="color output")
    parser.add_argument(
        "--ldflag",
        dest="ldflags",
        action="append",
        default=["-lkernel32"],
        help="extra linker flag",
    )
    args = parser.parse_args(argv)
    args.color = args.color == "always" or (args.color == "auto" and sys.stdout.isatty())

    try:
        args.std_objs = find_std_objects(args.vxc)
    except TestError as e:
        print(f"error: {e}", file=sys.stderr)
        return 2

    tests = collect_tests(args.paths)
    if not tests:
        print("error: no tests found", file=sys.stderr)
        return 2

    failures = []
    skipped = 0
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, args.jobs)) as executor:
        futures = [executor.submit(run_test, test, args) for test in tests]
        for future in concurrent.futures.as_completed(futures):
            name, status, message = future.result()
            if status == "ok":
                print(f"{color('ok', GREEN, args.color)}   {name}")
            elif status == "skip":
                print(f"{color('skip', YELLOW, args.color)} {name}")
                skipped += 1
            else:
                print(f"{color('fail', RED, args.color)} {name}")
                failures.append((name, message))

    if failures:
        print()
        for name, message in failures:
            print(f"== {name} ==")
            print(message)
        print()
        print(f"{len(tests) - len(failures) - skipped}/{len(tests)} tests passed, {skipped} skipped")
        return 1

    print(f"{len(tests) - skipped}/{len(tests)} tests passed, {skipped} skipped")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
