#!/usr/bin/env python3
"""Build the pinned fzf benchmark subject and record the exact build."""

from __future__ import annotations

import argparse
import datetime
import hashlib
import json
import os
import pathlib
import shutil
import subprocess
import tempfile


PINNED_FZF_COMMIT = "63e82a9e3dd52cc67a46db842b8a29e0c1f83229"


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
    parser.add_argument("--fzf-source", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    parser.add_argument("--provenance", type=pathlib.Path, required=True)
    parser.add_argument("--go", default="go")
    arguments = parser.parse_args()

    source = arguments.fzf_source.resolve()
    binary = arguments.output.resolve()
    provenance_path = arguments.provenance.resolve()
    go = pathlib.Path(shutil.which(arguments.go) or arguments.go).resolve()
    patch = pathlib.Path(__file__).with_name("fzf-bench-json.patch").resolve()
    patch_bytes = patch.read_bytes()
    patch_sha256 = hashlib.sha256(patch_bytes).hexdigest()

    revision = output(["git", "-C", str(source), "rev-parse", "HEAD"])
    if revision != PINNED_FZF_COMMIT:
        raise RuntimeError(
            f"fzf source is {revision}, expected {PINNED_FZF_COMMIT}"
        )
    if output(["git", "-C", str(source), "status", "--porcelain"]):
        raise RuntimeError("fzf source is not clean")
    pinned_tree = output([
        "git", "-C", str(source), "rev-parse",
        f"{PINNED_FZF_COMMIT}^{{tree}}",
    ])

    query_keys = [
        "GOOS", "GOARCH", "CGO_ENABLED", "GOCACHE", "GOMODCACHE", "GOPATH",
        "GOPROXY", "GOSUMDB", "GONOSUMDB", "GOPRIVATE", "GONOPROXY",
        "GOENV", "GOFLAGS", "GOTOOLCHAIN", "GOWORK", "GOVERSION",
    ]
    host_go = json.loads(output([str(go), "env", "-json", *query_keys]))
    build_environment = {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": os.environ.get("HOME", ""),
        "TMPDIR": os.environ.get("TMPDIR", "/tmp"),
        "CGO_ENABLED": "0",
        "GOOS": host_go["GOOS"],
        "GOARCH": host_go["GOARCH"],
        "GOCACHE": host_go["GOCACHE"],
        "GOMODCACHE": host_go["GOMODCACHE"],
        "GOPATH": host_go["GOPATH"],
        "GOPROXY": host_go["GOPROXY"],
        "GOSUMDB": host_go["GOSUMDB"],
        "GONOSUMDB": host_go["GONOSUMDB"],
        "GOPRIVATE": host_go["GOPRIVATE"],
        "GONOPROXY": host_go["GONOPROXY"],
        "GOENV": "off",
        "GOFLAGS": "",
        "GOTOOLCHAIN": "local",
        "GOWORK": "off",
    }

    binary.parent.mkdir(parents=True, exist_ok=True)
    provenance_path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="fzf-bench-build-") as directory:
        checkout = pathlib.Path(directory) / "fzf"
        frozen_patch = pathlib.Path(directory) / "fzf-bench-json.patch"
        frozen_patch.write_bytes(patch_bytes)
        build_cwd = str(checkout)
        subprocess.run(
            ["git", "-C", str(source), "worktree", "add", "--detach",
             str(checkout), PINNED_FZF_COMMIT],
            check=True,
        )
        try:
            subprocess.run(
                ["git", "-C", str(checkout), "apply", str(frozen_patch)],
                check=True,
            )
            build_argv = [
                str(go), "build", "-trimpath", "-buildvcs=false",
                "-o", str(binary), ".",
            ]
            subprocess.run(
                build_argv, cwd=checkout, env=build_environment, check=True
            )
            effective_go = json.loads(output(
                [str(go), "env", "-json", *query_keys],
                cwd=checkout,
                env=build_environment,
            ))
            patched_core_sha256 = sha256(checkout / "src/core.go")
        finally:
            subprocess.run(
                ["git", "-C", str(source), "worktree", "remove", "--force",
                 str(checkout)],
                check=True,
            )

    build_info = output([str(go), "version", "-m", str(binary)])
    provenance = {
        "schema": 1,
        "recorded_utc": datetime.datetime.now(
            datetime.timezone.utc
        ).isoformat().replace("+00:00", "Z"),
        "fzf_commit": revision,
        "fzf_tree": pinned_tree,
        "patch": str(patch),
        "patch_sha256": patch_sha256,
        "patched_core_sha256": patched_core_sha256,
        "build_cwd": build_cwd,
        "cwd_kind": "temporary detached worktree at the pinned commit",
        "build_argv": build_argv,
        "build_environment": build_environment,
        "effective_go_environment": effective_go,
        "binary": str(binary),
        "binary_sha256": sha256(binary),
        "go_version_m": build_info.splitlines(),
    }
    provenance_path.write_text(
        json.dumps(provenance, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
