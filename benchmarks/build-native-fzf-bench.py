#!/usr/bin/env python3
"""Build one native scan benchmark and record its exact inputs."""

from __future__ import annotations

import argparse
import datetime
import hashlib
import json
import os
import pathlib
import shutil
import subprocess


BUILD_OPTIONS = [
    "-std=gnu11", "-Wall", "-Wextra", "-O3", "-DNDEBUG", "-pthread",
]

PRODUCT_SOURCES = [
    "fzf.c",
    "fzf.h",
    "fzf-private.h",
    "fzf-simd-prefilter.h",
    "fzf-normalize.inc",
    "fzf-score-input.inc",
    "utf8_char_index.h",
    "utf8proc-2.10.0/utf8proc.c",
    "utf8proc-2.10.0/utf8proc.h",
    "utf8proc-2.10.0/utf8proc_data.c",
]


def utc_now() -> str:
    return datetime.datetime.now(
        datetime.timezone.utc
    ).isoformat().replace("+00:00", "Z")


def sha256(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def output(argv: list[str], **kwargs: object) -> str:
    return subprocess.check_output(argv, text=True, **kwargs).strip()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    parser.add_argument("--provenance", type=pathlib.Path, required=True)
    parser.add_argument("--cc", default="cc")
    parser.add_argument(
        "--driver",
        type=pathlib.Path,
        default=pathlib.Path(__file__).with_name("fzf-bench-driver.c"),
    )
    arguments = parser.parse_args()

    source = arguments.source.resolve()
    binary = arguments.output.resolve()
    provenance_path = arguments.provenance.resolve()
    driver = arguments.driver.resolve()
    compiler = pathlib.Path(
        shutil.which(arguments.cc) or arguments.cc
    ).resolve()
    compiler_hash = sha256(compiler)

    source_commit = output(["git", "-C", str(source), "rev-parse", "HEAD"])
    source_tree = output([
        "git", "-C", str(source), "rev-parse", "HEAD^{tree}",
    ])
    if output(["git", "-C", str(source), "status", "--porcelain"]):
        raise RuntimeError("native product source is not clean")

    driver_root = pathlib.Path(output([
        "git", "-C", str(driver.parent), "rev-parse", "--show-toplevel",
    ]))
    driver_relative = driver.relative_to(driver_root)
    driver_commit = output([
        "git", "-C", str(driver_root), "rev-parse", "HEAD",
    ])
    driver_tree = output([
        "git", "-C", str(driver_root), "rev-parse", "HEAD^{tree}",
    ])
    if output([
        "git", "-C", str(driver_root), "status", "--porcelain", "--",
        str(driver_relative),
    ]):
        raise RuntimeError("benchmark driver is not clean")

    product_paths = {name: source / name for name in PRODUCT_SOURCES}
    missing = [name for name, path in product_paths.items() if not path.is_file()]
    if missing:
        raise RuntimeError(f"missing native product sources: {', '.join(missing)}")
    product_hashes = {
        name: sha256(path) for name, path in product_paths.items()
    }
    driver_hash = sha256(driver)

    build_environment = {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": os.environ.get("HOME", ""),
        "TMPDIR": os.environ.get("TMPDIR", "/tmp"),
        "LC_ALL": "C",
        "LANG": "C",
        "TZ": "UTC",
    }
    compiler_version = subprocess.run(
        [str(compiler), "--version"], capture_output=True, text=True,
        env=build_environment, check=True,
    )

    binary.parent.mkdir(parents=True, exist_ok=True)
    provenance_path.parent.mkdir(parents=True, exist_ok=True)
    build_argv = [
        str(compiler), *BUILD_OPTIONS,
        f"-I{source}", f"-I{source / 'utf8proc-2.10.0'}",
        "-o", str(binary), str(driver), str(source / "fzf.c"),
        str(source / "utf8proc-2.10.0/utf8proc.c"),
    ]
    completed = subprocess.run(
        build_argv, capture_output=True, text=True,
        env=build_environment, check=True,
    )

    if sha256(driver) != driver_hash or {
        name: sha256(path) for name, path in product_paths.items()
    } != product_hashes:
        raise RuntimeError("native benchmark source changed during the build")
    if output(["git", "-C", str(source), "rev-parse", "HEAD"]) != source_commit:
        raise RuntimeError("native product revision changed during the build")
    if output(["git", "-C", str(source), "status", "--porcelain"]):
        raise RuntimeError("native product source changed during the build")
    if sha256(compiler) != compiler_hash:
        raise RuntimeError("compiler changed during the build")

    provenance = {
        "schema": 1,
        "kind": "fzf-native-scan-benchmark-build",
        "recorded_utc": utc_now(),
        "source": str(source),
        "source_commit": source_commit,
        "source_tree": source_tree,
        "source_status": "clean",
        "product_source_sha256": product_hashes,
        "driver": str(driver),
        "driver_commit": driver_commit,
        "driver_tree": driver_tree,
        "driver_source_sha256": driver_hash,
        "compiler": str(compiler),
        "compiler_sha256": compiler_hash,
        "compiler_version_argv": [str(compiler), "--version"],
        "compiler_version_stdout": compiler_version.stdout,
        "compiler_version_stderr": compiler_version.stderr,
        "build_options": BUILD_OPTIONS,
        "build_cwd": str(pathlib.Path.cwd()),
        "build_argv": build_argv,
        "build_environment": build_environment,
        "build_stdout": completed.stdout,
        "build_stderr": completed.stderr,
        "binary": str(binary),
        "binary_sha256": sha256(binary),
    }
    provenance_path.write_text(
        json.dumps(provenance, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
