#!/usr/bin/env python3
"""Compare the benchmark adapter with a pinned upstream fzf semantic oracle."""

import argparse
import difflib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile


PINNED_FZF_COMMIT = "63e82a9e3dd52cc67a46db842b8a29e0c1f83229"


def cases():
    fallback_long = "a" + "x" * 60_000 + "b c"
    fallback_short = "za" + "x" * 60 + "b" + "x" * 20 + "c"
    return [
        {
            "id": "duplicates-and-score-ties",
            "query": "abc",
            "normalize": False,
            "candidates": [
                "abc", "abc", "a-b-c", "ABC", "zabc", "abbc",
                "nope", "abc",
            ],
        },
        {
            "id": "compound-and-inverse",
            "query": "foo !bar",
            "normalize": False,
            "candidates": [
                "foo", "foo bar", "xxfooz", "bar foo", "FOO", "baz",
                "a foo value", "a foo value",
            ],
        },
        {
            "id": "or-terms",
            "query": "foo | baz",
            "normalize": False,
            "candidates": [
                "foo", "baz", "food", "embaz", "neither", "foo baz",
                "baz", "FOO",
            ],
        },
        {
            "id": "exact-term",
            "query": "'foo",
            "normalize": False,
            "candidates": ["foo", "xxfooz", "f-o-o", "FOO", "bar", "foo"],
        },
        {
            "id": "prefix-term",
            "query": "^foo",
            "normalize": False,
            "candidates": ["foo", "foobar", "afoo", "FOO", " foo", "foo"],
        },
        {
            "id": "suffix-term",
            "query": "bar$",
            "normalize": False,
            "candidates": ["bar", "foobar", "barx", "BAR", "bar ", "bar"],
        },
        {
            "id": "equal-term",
            "query": "^whole$",
            "normalize": False,
            "candidates": ["whole", "wholex", "xwhole", "WHOLE", "whole"],
        },
        {
            "id": "exact-boundary-term",
            "query": "'word'",
            "normalize": False,
            "candidates": [
                "word", "a word", "sword", "word-boundary", "WORD", "word",
            ],
        },
        {
            "id": "normalization-enabled",
            "query": "cafe",
            "normalize": True,
            "candidates": ["cafe", "café", "CAFÉ", "cafeteria", "caff", "café"],
        },
        {
            "id": "normalization-disabled",
            "query": "cafe",
            "normalize": False,
            "candidates": ["cafe", "café", "CAFÉ", "cafeteria", "caff", "café"],
        },
        {
            "id": "case-smart-respect",
            "query": "Foo",
            "normalize": False,
            "candidates": ["Foo", "foo", "xFoo", "FOO", "Foo", "food"],
        },
        {
            "id": "unicode-case-ignore",
            "query": "ång",
            "normalize": False,
            "candidates": ["ångström", "Ångström", "xång", "ang", "ång"],
        },
        {
            "id": "unicode-other-letter-score-rows",
            "query": "中文",
            "normalize": False,
            "candidates": [
                "中文", "中-文", "前中文", "中文字", "文中", "中文",
            ],
        },
        {
            "id": "inverse-only",
            "query": "!drop",
            "normalize": False,
            "candidates": ["keep", "drop", "also keep", "DROP", "keep"],
        },
        {
            "id": "v2-to-v1-fallback-negative-raw-score",
            "query": "ab c",
            "normalize": False,
            "candidates": [fallback_long, fallback_short, "does not match"],
        },
    ]


def run(command, *, cwd=None, env=None, input_bytes=None):
    completed = subprocess.run(
        command,
        cwd=cwd,
        env=env,
        input=input_bytes,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if completed.returncode != 0:
        raise RuntimeError(
            f"command failed ({completed.returncode}): {command!r}\n"
            f"stdout:\n{completed.stdout.decode(errors='replace')}\n"
            f"stderr:\n{completed.stderr.decode(errors='replace')}"
        )
    return completed.stdout


def require_pinned_source(source):
    if not (source / "go.mod").is_file() or not (source / "src").is_dir():
        raise RuntimeError(f"not an fzf source tree: {source}")
    commit = run(["git", "rev-parse", "HEAD"], cwd=source).decode().strip()
    if commit != PINNED_FZF_COMMIT:
        raise RuntimeError(
            f"FZF_SOURCE must be pinned to {PINNED_FZF_COMMIT}; got {commit}"
        )
    status = run(
        ["git", "status", "--porcelain", "--untracked-files=all"],
        cwd=source,
    ).decode()
    if status:
        raise RuntimeError("FZF_SOURCE must be clean; git status was:\n" + status)


def run_upstream_oracle(source, helper, payload, temporary):
    input_path = temporary / "cases.json"
    output_path = temporary / "upstream.json"
    overlay_path = temporary / "overlay.json"
    go_cache = temporary / "go-cache"
    go_tmp = temporary / "go-tmp"
    go_cache.mkdir()
    go_tmp.mkdir()
    input_path.write_text(json.dumps(payload, ensure_ascii=False) + "\n")
    injected_path = (source / "src" / "fzf_native_semantic_oracle_test.go").resolve()
    overlay_path.write_text(json.dumps({
        "Replace": {str(injected_path): str(helper.resolve())}
    }) + "\n")

    environment = os.environ.copy()
    environment.update({
        "FZF_SEMANTIC_ORACLE_INPUT": str(input_path),
        "FZF_SEMANTIC_ORACLE_OUTPUT": str(output_path),
        "GOPROXY": "off",
        "GOSUMDB": "off",
        "GOTOOLCHAIN": "local",
        "GOCACHE": str(go_cache),
        "GOTMPDIR": str(go_tmp),
    })
    run([
        "go", "test", "-overlay", str(overlay_path), "./src",
        "-run", "^TestFzfNativeSemanticOracle$", "-count=1",
    ], cwd=source, env=environment)
    return json.loads(output_path.read_text())


def parse_native_record(line):
    fields = line.split("\t")
    if len(fields) != 11:
        raise RuntimeError(f"bad native semantic record: {line!r}")
    values = [int(value) for value in fields]
    return {
        "index": values[0],
        "raw_score": values[1],
        "min_begin": values[2],
        "min_end": values[3],
        "max_end": values[4],
        "bounds_valid": bool(values[5]),
        "rank_score": values[6],
        "points": values[7:11],
    }


def run_native_oracle(binary, oracle_case):
    command = [
        str(binary), f"--filter={oracle_case['query']}",
        "--tiebreak=index", "--threads=1", "--algo=v2", "--sort",
        "--no-literal" if oracle_case["normalize"] else "--literal",
        "--dump-semantic",
    ]
    corpus = "".join(candidate + "\n" for candidate in oracle_case["candidates"])
    stdout = run(command, input_bytes=corpus.encode("utf-8"))
    return [
        parse_native_record(line)
        for line in stdout.decode().splitlines()
        if line
    ]


def compare(native_binary, payload, upstream):
    upstream_by_id = {case["id"]: case["records"] for case in upstream["cases"]}
    failures = []
    for oracle_case in payload["cases"]:
        case_id = oracle_case["id"]
        native = run_native_oracle(native_binary, oracle_case)
        expected = upstream_by_id.get(case_id)
        if native != expected:
            expected_text = json.dumps(expected, indent=2, sort_keys=True).splitlines()
            native_text = json.dumps(native, indent=2, sort_keys=True).splitlines()
            failures.append("\n".join(difflib.unified_diff(
                expected_text, native_text,
                fromfile=f"upstream/{case_id}",
                tofile=f"native/{case_id}",
                lineterm="",
            )))
        else:
            print(f"semantic parity {case_id}: {len(native)} matches")

    fallback = upstream_by_id["v2-to-v1-fallback-negative-raw-score"]
    if not any(record["raw_score"] < 0 for record in fallback):
        failures.append("fallback fixture did not produce a negative raw score")
    if failures:
        raise RuntimeError("semantic parity failed:\n" + "\n\n".join(failures))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--fzf-source", required=True, type=Path)
    parser.add_argument("--native-driver", required=True, type=Path)
    arguments = parser.parse_args()

    source = arguments.fzf_source.resolve()
    native_binary = arguments.native_driver.resolve()
    helper = Path(__file__).with_name("fzf-upstream-semantic-oracle_test.go")
    require_pinned_source(source)
    if not native_binary.is_file():
        raise RuntimeError(f"native driver does not exist: {native_binary}")

    payload = {"cases": cases()}
    with tempfile.TemporaryDirectory(prefix="fzf-semantic-parity-") as directory:
        upstream = run_upstream_oracle(source, helper, payload, Path(directory))
    compare(native_binary, payload, upstream)
    print(
        f"semantic parity: {len(payload['cases'])} cases agree with "
        f"fzf {PINNED_FZF_COMMIT}"
    )


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"semantic parity: FAIL: {error}", file=sys.stderr)
        raise SystemExit(1)
