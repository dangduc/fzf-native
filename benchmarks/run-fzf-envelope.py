#!/usr/bin/env python3
"""Run paired fzf and fzf-native scan-envelope benchmarks."""

from __future__ import annotations

import argparse
import datetime
import hashlib
import json
import os
import pathlib
import platform
import random
import subprocess
import sys


DEFAULT_SEED = 0xF2F20260909
PINNED_FZF_COMMIT = "63e82a9e3dd52cc67a46db842b8a29e0c1f83229"
BENCH_PREFIX = "benchmark-json "
SEMANTIC_PREFIX = "semantic "

LANES = {
    "default-length-w1": {
        "profile": "default-length", "threads": 1, "primary": True,
        "arguments": ["--tiebreak=length"],
    },
    "default-length-w8": {
        "profile": "default-length", "threads": 8, "primary": True,
        "arguments": ["--tiebreak=length"],
    },
    "literal-index-w1": {
        "profile": "literal-index", "threads": 1, "primary": False,
        "arguments": ["--literal", "--tiebreak=index"],
    },
    "literal-index-w8": {
        "profile": "literal-index", "threads": 8, "primary": False,
        "arguments": ["--literal", "--tiebreak=index"],
    },
}

CORPUS_SHA256 = {
    "chromium": "46163ffa59aa350386dfa8447a898660a5ff02054cbcae7d1d3d96943582bbc3",
    "arabic": "e107f84c8c5d7cf468c3e33b5fa894056fadce8da4232f71a04aa4b4e4fefbbc",
    "korean": "9a3042337b54d83281ed51d347dd6817912a71723c392529dbb21def73c10296",
    "partial-16": "0e61944de50783c147e12b073ba18358d6a45bf4d46132c6194f04c325ff6800",
    "partial-32": "aec093c49d3e54447e46c83956b095246758c0f1693e6d9d63097ff617606960",
    "partial-64": "3f7097d05461de95c1e0634a1dfd4a5b36560538f5a4e73d09382f4b73c8fb4d",
    "partial-128": "6227f21670c010b61f57f553ddd8044921ff471a167776265dccf99721006ce1",
    "all-16": "c6a8f7ea821806906a21cb0f614696c62b1d9e4fa62e1f06d77c645f7a98afdd",
    "all-32": "e0fd4573b92412754df8c63a930a2d147d9d3d5ae9573ec0b407d1eff7f191b7",
    "all-64": "fa319758f69d2c9ee41307cb5f9c44dff9e9b25980c7383117cb24ce16aa9da1",
    "all-128": "3fdf3aaa864508d87738ea1dc36c294cf6075714ca4fc41935cd8e478bb6b416",
    "partial-miss-16": "a59bda342eba5478a3b905b29224ca081d767adcc5329959c43d04329ed3ce29",
    "partial-miss-32": "2f2aa8282a071e62e69f3d77131b64e82ac25820298530d7d4a25fab4e97faf4",
    "partial-miss-64": "efe7f1ac9c62921b31de27a4432c9700c708ccff3077b5c8168456ff2761160d",
    "partial-miss-128": "e67861a7b4cb5b41717154b8f8723c85386799fb9e84b50344430328b088816a",
    "none-16": "5705157daafba71e6b409fcbd44d2ddfd30eac249457195526dcc56a6a20ee16",
    "none-32": "d40b5466270e6c90a733744597a3004cdef4ed8985b097a549bd84b9496abe3f",
    "none-64": "cb0682a57e35f5542f2f024c9d1280b6b1c7e8f26338087a5fa0f68fd0b60781",
    "none-128": "9b9c06b5560975f727e75b5d870f479927134ad1b275a72e4bee541a51e56558",
}


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


def cells(corpus_root: pathlib.Path) -> dict[str, dict[str, object]]:
    result: dict[str, dict[str, object]] = {
        "chromium": {
            "path": corpus_root / "chromium.txt", "items": 1_406_940,
            "matches": 179_966, "query": "linux",
        },
        "arabic": {
            "path": corpus_root / "arabic_unicode.txt", "items": 285_587,
            "matches": 22_659, "query": "إن",
        },
        "korean": {
            "path": corpus_root / "korean_unicode.txt", "items": 281_471,
            "matches": 23_696, "query": "니다",
        },
    }
    for family in ("partial", "all", "partial-miss", "none"):
        for length in (16, 32, 64, 128):
            name = f"{family}-{length}"
            result[name] = {
                "path": corpus_root / "synthetic" / f"{name}.txt",
                "items": 100_000,
                "matches": 4_977 if family == "partial" else (
                    100_000 if family == "all" else 0
                ),
                "query": "deadbeef",
            }
    return result


def parse_machine_output(
    subject: str, output: str, expected: dict[str, object]
) -> dict[str, object]:
    lines = [line for line in output.splitlines() if line]
    machine = [line for line in lines if line.startswith(BENCH_PREFIX)]
    semantic = [line for line in lines if line.startswith(SEMANTIC_PREFIX)]
    if len(machine) != 1:
        raise RuntimeError(f"missing machine statistics for {subject}: {output!r}")
    required_lines = 2 if subject != "fzf" else 1
    if len(lines) != required_lines:
        raise RuntimeError(f"unexpected output for {subject}: {output!r}")
    if (subject == "fzf" and semantic) or (
        subject != "fzf" and len(semantic) != 1
    ):
        raise RuntimeError(f"unexpected semantic output for {subject}")

    stats = json.loads(machine[0][len(BENCH_PREFIX):])
    required = {
        "schema", "iterations", "total_ns", "min_ns", "max_ns",
        "items", "matches", "ingestion_ns",
    }
    if set(stats) != required or any(
        type(stats[field]) is not int for field in required
    ) or stats["schema"] != 1:
        raise RuntimeError(f"unexpected statistics schema: {stats!r}")
    nonnegative = required - {"schema", "iterations", "total_ns"}
    if any(stats[field] < 0 for field in nonnegative):
        raise RuntimeError(f"negative statistics: {stats!r}")
    if stats["iterations"] < 1 or stats["total_ns"] < 1:
        raise RuntimeError(f"invalid iteration statistics: {stats!r}")
    if stats["min_ns"] > stats["max_ns"]:
        raise RuntimeError(f"invalid range: {stats!r}")
    if not (
        stats["min_ns"] * stats["iterations"] <= stats["total_ns"]
        <= stats["max_ns"] * stats["iterations"]
    ):
        raise RuntimeError(f"total outside sample bounds: {stats!r}")
    average_ns = stats["total_ns"] / stats["iterations"]
    if (stats["items"], stats["matches"]) != (
        expected["items"], expected["matches"]
    ):
        raise RuntimeError(f"count mismatch: {stats!r}")

    result: dict[str, object] = {**stats, "average_ns": average_ns}
    if semantic:
        fields = dict(part.split("=", 1) for part in semantic[0].split()[1:])
        if set(fields) != {
            "items", "matches", "input_checksum", "result_checksum"
        }:
            raise RuntimeError(f"unexpected semantic schema: {semantic[0]!r}")
        result["semantic"] = {
            "items": int(fields["items"]),
            "matches": int(fields["matches"]),
            "input_checksum": fields["input_checksum"],
            "result_checksum": fields["result_checksum"],
        }
        if (
            result["semantic"]["items"], result["semantic"]["matches"]
        ) != (stats["items"], stats["matches"]):
            raise RuntimeError("native semantic counts differ from statistics")
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--artifact-root", type=pathlib.Path, required=True)
    parser.add_argument("--corpus-root", type=pathlib.Path, required=True)
    parser.add_argument("--candidate", type=pathlib.Path, required=True)
    parser.add_argument("--base", type=pathlib.Path, required=True)
    parser.add_argument("--fzf", type=pathlib.Path, required=True)
    parser.add_argument(
        "--fzf-build-provenance", type=pathlib.Path, required=True
    )
    parser.add_argument("--rounds", type=int, default=10)
    parser.add_argument("--duration", default="1s")
    parser.add_argument("--seed", type=lambda value: int(value, 0), default=DEFAULT_SEED)
    parser.add_argument("--lane", action="append", choices=sorted(LANES))
    parser.add_argument("--cell", action="append")
    arguments = parser.parse_args()
    if arguments.rounds < 2 or arguments.rounds % 2:
        raise RuntimeError("--rounds must be a positive even number of at least 2")

    subjects = {
        "candidate": arguments.candidate.resolve(),
        "base": arguments.base.resolve(),
        "fzf": arguments.fzf.resolve(),
    }
    fzf_provenance_path = arguments.fzf_build_provenance.resolve()
    selected_lanes = arguments.lane or list(LANES)
    if len(selected_lanes) != len(set(selected_lanes)):
        raise RuntimeError("--lane values must be unique")
    all_cells = cells(arguments.corpus_root.resolve())
    selected_cells = arguments.cell or list(all_cells)
    if len(selected_cells) != len(set(selected_cells)):
        raise RuntimeError("--cell values must be unique")
    unknown_cells = sorted(set(selected_cells) - set(all_cells))
    if unknown_cells:
        raise RuntimeError(f"unknown cells: {', '.join(unknown_cells)}")
    workload_cells = {name: all_cells[name] for name in selected_cells}
    subject_hashes = {name: sha256(path) for name, path in subjects.items()}
    fzf_provenance = json.loads(
        fzf_provenance_path.read_text(encoding="utf-8")
    )
    if fzf_provenance.get("schema") != 1:
        raise RuntimeError("unsupported fzf build provenance schema")
    if fzf_provenance.get("fzf_commit") != PINNED_FZF_COMMIT:
        raise RuntimeError("fzf build provenance does not name the pinned commit")
    expected_patch = pathlib.Path(__file__).with_name(
        "fzf-bench-json.patch"
    ).resolve()
    if fzf_provenance.get("patch_sha256") != sha256(expected_patch):
        raise RuntimeError("fzf build provenance does not use this benchmark patch")
    if fzf_provenance.get("binary_sha256") != subject_hashes["fzf"]:
        raise RuntimeError("fzf binary does not match its build provenance")
    if not isinstance(fzf_provenance.get("build_argv"), list) or not isinstance(
        fzf_provenance.get("build_environment"), dict
    ):
        raise RuntimeError("fzf build provenance lacks build argv or environment")
    corpus_hashes = {
        name: sha256(cell["path"]) for name, cell in workload_cells.items()
    }
    if corpus_hashes != {
        name: CORPUS_SHA256[name] for name in workload_cells
    }:
        raise RuntimeError("one or more corpora do not match the frozen hashes")

    root = arguments.artifact_root.resolve()
    root.mkdir(parents=True, exist_ok=True)
    for path in (
        root / "campaign.json", root / "commands.jsonl",
        root / "ordered-output.json", root / "ordered-output",
        root / "timings",
    ):
        if path.exists():
            raise RuntimeError(f"refusing to replace campaign artifact: {path}")
    ordered_output_root = root / "ordered-output"
    ordered_output_root.mkdir()
    logs = root / "timings"
    logs.mkdir()

    environment = {
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "HOME": os.environ.get("HOME", ""),
        "TMPDIR": os.environ.get("TMPDIR", "/tmp"),
        "LC_ALL": "C",
        "LANG": "C",
        "TZ": "UTC",
        "FZF_BENCH_JSON": "1",
    }

    schedule = []
    schedule_randomizer = random.Random(arguments.seed)
    subject_orders = {
        (pair, cell, lane): list(subjects)
        for pair in range(1, arguments.rounds // 2 + 1)
        for cell in workload_cells
        for lane in selected_lanes
    }
    for key, order in subject_orders.items():
        random.Random(f"{arguments.seed}:{key}").shuffle(order)
    for round_number in range(1, arguments.rounds + 1):
        names = list(workload_cells)
        schedule_randomizer.shuffle(names)
        for cell in names:
            lane_names = selected_lanes.copy()
            schedule_randomizer.shuffle(lane_names)
            for lane_name in lane_names:
                pair = (round_number + 1) // 2
                base_order = subject_orders[(pair, cell, lane_name)]
                order = base_order.copy() if round_number % 2 else list(
                    reversed(base_order)
                )
                schedule.append((round_number, cell, lane_name, order))

    profiles: dict[str, dict[str, object]] = {}
    for lane_name in selected_lanes:
        lane = LANES[lane_name]
        profile_name = str(lane["profile"])
        profile = profiles.setdefault(
            profile_name,
            {
                "arguments": list(lane["arguments"]),
                "threads": lane["threads"],
            },
        )
        if profile["arguments"] != lane["arguments"]:
            raise RuntimeError(f"inconsistent lane arguments for {profile_name}")
        profile["threads"] = min(int(profile["threads"]), int(lane["threads"]))

    started = utc_now()
    ordered_records = []
    for cell_name, cell in workload_cells.items():
        for profile_name, profile in profiles.items():
            output_hashes = set()
            for subject, binary in subjects.items():
                command = [
                    str(binary), f"--filter={cell['query']}",
                    f"--threads={profile['threads']}", "--algo=v2",
                    "--scheme=default", "--sort", *profile["arguments"],
                ]
                if subject != "fzf":
                    command.append("--dump-results")
                stem = f"{profile_name}-{cell_name}-{subject}"
                stdout_path = ordered_output_root / f"{stem}.stdout"
                stderr_path = ordered_output_root / f"{stem}.stderr"
                command_started = utc_now()
                with cell["path"].open("rb") as source:
                    completed = subprocess.run(
                        command, stdin=source, capture_output=True,
                        env=environment,
                    )
                stdout_path.write_bytes(completed.stdout)
                stderr_path.write_bytes(completed.stderr)
                expected_returncode = (
                    1 if subject == "fzf" and cell["matches"] == 0 else 0
                )
                if completed.returncode != expected_returncode or completed.stderr:
                    raise RuntimeError(
                        f"ordered-output process failed: {stem} "
                        f"rc={completed.returncode} stderr={completed.stderr!r}"
                    )
                line_count = len(completed.stdout.splitlines())
                if line_count != cell["matches"]:
                    raise RuntimeError(
                        f"ordered-output count mismatch: {stem} "
                        f"got={line_count} expected={cell['matches']}"
                    )
                output_hash = sha256(stdout_path)
                output_hashes.add(output_hash)
                ordered_records.append({
                    "profile": profile_name,
                    "threads": profile["threads"],
                    "arguments": profile["arguments"],
                    "cell": cell_name,
                    "query": cell["query"],
                    "corpus": str(cell["path"]),
                    "subject": subject,
                    "argv": command,
                    "started_utc": command_started,
                    "completed_utc": utc_now(),
                    "returncode": completed.returncode,
                    "line_count": line_count,
                    "stdout": str(stdout_path),
                    "stderr": str(stderr_path),
                    "stdout_sha256": output_hash,
                    "stderr_sha256": sha256(stderr_path),
                })
            if len(output_hashes) != 1:
                raise RuntimeError(
                    f"ordered output differs: {cell_name} {profile_name}"
                )
    ordered_output_path = root / "ordered-output.json"
    ordered_output_path.write_text(
        json.dumps(ordered_records, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )

    # Compare each native process with the first native signature for its
    # workload and profile. The full-corpus gate above compares with fzf.
    native_signatures: dict[tuple[str, str], dict[str, object]] = {}
    records = []
    commands_path = root / "commands.jsonl"
    with commands_path.open("x", encoding="utf-8") as command_log:
        for round_number, cell_name, lane_name, subject_order in schedule:
            lane = LANES[lane_name]
            cell = workload_cells[cell_name]
            for subject in subject_order:
                binary = subjects[subject]
                command = [
                    str(binary), f"--filter={cell['query']}",
                    f"--bench={arguments.duration}",
                    f"--threads={lane['threads']}", "--algo=v2",
                    "--scheme=default", "--sort", *lane["arguments"],
                ]
                stem = (
                    f"r{round_number:02d}-{lane_name}-{cell_name}-{subject}"
                )
                stdout_path = logs / f"{stem}.stdout"
                stderr_path = logs / f"{stem}.stderr"
                command_started = utc_now()
                with cell["path"].open("rb") as source:
                    completed = subprocess.run(
                        command, stdin=source, capture_output=True,
                        env=environment,
                    )
                stdout_path.write_bytes(completed.stdout)
                stderr_path.write_bytes(completed.stderr)
                if completed.returncode != 0 or completed.stderr:
                    raise RuntimeError(
                        f"process failed: {stem} rc={completed.returncode} "
                        f"stderr={completed.stderr!r}"
                    )
                parsed = parse_machine_output(
                    subject, completed.stdout.decode("utf-8"), cell
                )
                if subject != "fzf":
                    key = (cell_name, str(lane["profile"]))
                    signature = parsed["semantic"]
                    previous = native_signatures.setdefault(key, signature)
                    if signature != previous:
                        raise RuntimeError(
                            f"native semantic mismatch: {stem}"
                        )
                record = {
                    "round": round_number,
                    "lane": lane_name,
                    **lane,
                    "cell": cell_name,
                    "query": cell["query"],
                    "corpus": str(cell["path"]),
                    "subject": subject,
                    "argv": command,
                    "started_utc": command_started,
                    "completed_utc": utc_now(),
                    "stdout": str(stdout_path),
                    "stderr": str(stderr_path),
                    "stdout_sha256": sha256(stdout_path),
                    "stderr_sha256": sha256(stderr_path),
                    **parsed,
                }
                records.append(record)
                command_log.write(
                    json.dumps(record, ensure_ascii=False) + "\n"
                )
                command_log.flush()

    if {name: sha256(path) for name, path in subjects.items()} != subject_hashes:
        raise RuntimeError("subject binary changed during the campaign")
    if {
        name: sha256(cell["path"]) for name, cell in workload_cells.items()
    } != corpus_hashes:
        raise RuntimeError("corpus changed during the campaign")

    result = {
        "schema": 1,
        "status": "ordered-output-and-timings-complete",
        "started_utc": started,
        "completed_utc": utc_now(),
        "rounds": arguments.rounds,
        "duration": arguments.duration,
        "seed": arguments.seed,
        "runner": str(pathlib.Path(__file__).resolve()),
        "runner_sha256": sha256(pathlib.Path(__file__).resolve()),
        "runner_argv": sys.argv,
        "runner_cwd": str(pathlib.Path.cwd()),
        "python": sys.version,
        "host": {
            "platform": platform.platform(),
            "machine": platform.machine(),
        },
        "lanes": {name: LANES[name] for name in selected_lanes},
        "environment": environment,
        "subjects": {name: str(path) for name, path in subjects.items()},
        "subject_sha256": subject_hashes,
        "fzf_build_provenance": str(fzf_provenance_path),
        "fzf_build_provenance_sha256": sha256(fzf_provenance_path),
        "fzf_build": fzf_provenance,
        "cells": {
            name: {
                "path": str(cell["path"]),
                "items": cell["items"],
                "matches": cell["matches"],
                "query": cell["query"],
            }
            for name, cell in workload_cells.items()
        },
        "corpus_sha256": corpus_hashes,
        "ordered_output_checks": ordered_records,
        "ordered_output_sha256": sha256(ordered_output_path),
        "record_count": len(records),
        "records": records,
    }
    (root / "campaign.json").write_text(
        json.dumps(result, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"fzf envelope campaign: FAIL: {error}", file=sys.stderr)
        raise
