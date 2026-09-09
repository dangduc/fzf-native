#!/usr/bin/env python3
"""Analyze a completed fzf matched-envelope benchmark campaign.

The campaign runner randomizes process order, but emits one measurement for
each subject in every fixed (lane, cell, round) block.  This analyzer keeps
that pairing: it divides same-round average nanosecond measurements, takes the
median ratio for each fixed cell, and gives every cell equal weight when it
forms an aggregate.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import math
import pathlib
import random
import statistics
import sys
from typing import Iterable, Optional, Sequence


SUBJECTS = ("base", "candidate", "fzf")
RATIOS = {
    "base/candidate": ("base", "candidate"),
    "fzf/candidate": ("fzf", "candidate"),
    "fzf/base": ("fzf", "base"),
}
CATEGORY_ORDER = ("real", "partial", "all", "partial-miss", "none")
REAL_CELLS = frozenset(("chromium", "arabic", "korean"))
DEFAULT_BOOTSTRAP_SAMPLES = 10_000
DEFAULT_BOOTSTRAP_SEED = 0xA11CE20260909


def sha256(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def category_for_cell(name: str) -> str:
    if name in REAL_CELLS:
        return "real"
    if name.startswith("partial-miss-"):
        return "partial-miss"
    if name.startswith("partial-"):
        return "partial"
    if name.startswith("all-"):
        return "all"
    if name.startswith("none-"):
        return "none"
    raise ValueError(f"cannot assign benchmark category to cell {name!r}")


def percentile(sorted_values: Sequence[float], probability: float) -> float:
    """Return a linearly interpolated percentile from sorted values."""
    if not sorted_values:
        raise ValueError("a percentile needs at least one value")
    if not 0.0 <= probability <= 1.0:
        raise ValueError("percentile probability must be in [0, 1]")
    position = (len(sorted_values) - 1) * probability
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return sorted_values[lower]
    fraction = position - lower
    return (
        sorted_values[lower] * (1.0 - fraction)
        + sorted_values[upper] * fraction
    )


def geometric_mean(values: Iterable[float]) -> float:
    materialized = list(values)
    if not materialized:
        raise ValueError("a geometric mean needs at least one value")
    if any(value <= 0.0 or not math.isfinite(value) for value in materialized):
        raise ValueError("geometric mean values must be finite and positive")
    return math.exp(math.fsum(math.log(value) for value in materialized) /
                    len(materialized))


def stable_seed(seed: int, *parts: str) -> int:
    encoded = "\0".join((str(seed), *parts)).encode("utf-8")
    return int.from_bytes(hashlib.sha256(encoded).digest()[:16], "big")


def bootstrap_cell_medians(
    values: Sequence[float], samples: int, seed: int
) -> list[float]:
    if samples < 1:
        raise ValueError("bootstrap sample count must be positive")
    if not values:
        raise ValueError("bootstrap needs at least one observed value")
    generator = random.Random(seed)
    count = len(values)
    return [
        statistics.median(generator.choice(values) for _ in range(count))
        for _ in range(samples)
    ]


def confidence_interval(values: Sequence[float]) -> dict[str, float]:
    ordered = sorted(values)
    return {
        "lower": percentile(ordered, 0.025),
        "upper": percentile(ordered, 0.975),
    }


def ratio_summary(
    estimate: float, bootstrap_values: Sequence[float]
) -> dict[str, object]:
    return {
        "estimate": estimate,
        "ci95": confidence_interval(bootstrap_values),
        "bootstrap_samples": len(bootstrap_values),
    }


def average_ns(record: dict[str, object]) -> float:
    value = record.get("average_ns")
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("timing record lacks numeric average_ns")
    value = float(value)
    if value <= 0.0 or not math.isfinite(value):
        raise ValueError("average_ns must be finite and positive")

    iterations = record.get("iterations")
    total_ns = record.get("total_ns")
    if type(iterations) is not int or type(total_ns) is not int:
        raise ValueError("timing record lacks exact iterations or total_ns")
    if iterations < 1 or total_ns < 1:
        raise ValueError("iterations and total_ns must be positive")
    exact_average = total_ns / iterations
    if not math.isclose(value, exact_average, rel_tol=1e-15, abs_tol=0.0):
        raise ValueError(
            f"average_ns {value!r} differs from total_ns/iterations "
            f"{exact_average!r}"
        )
    return value


def validate_campaign(campaign: dict[str, object]) -> None:
    if campaign.get("schema") != 1:
        raise ValueError("unsupported campaign schema")
    if campaign.get("status") != "ordered-output-and-timings-complete":
        raise ValueError("campaign did not complete ordered-output and timings")
    rounds = campaign.get("rounds")
    if type(rounds) is not int or rounds < 2:
        raise ValueError("campaign rounds must be an integer of at least 2")
    lanes = campaign.get("lanes")
    cells = campaign.get("cells")
    records = campaign.get("records")
    if not isinstance(lanes, dict) or not lanes:
        raise ValueError("campaign has no lanes")
    if not isinstance(cells, dict) or not cells:
        raise ValueError("campaign has no cells")
    if not isinstance(records, list):
        raise ValueError("campaign records must be a list")
    if campaign.get("record_count") != len(records):
        raise ValueError("campaign record_count differs from records length")

    expected = rounds * len(lanes) * len(cells) * len(SUBJECTS)
    if len(records) != expected:
        raise ValueError(
            f"campaign has {len(records)} timing records; expected {expected}"
        )


def index_records(
    campaign: dict[str, object]
) -> dict[tuple[str, str, int], dict[str, dict[str, object]]]:
    lanes = campaign["lanes"]
    cells = campaign["cells"]
    rounds = campaign["rounds"]
    assert isinstance(lanes, dict)
    assert isinstance(cells, dict)
    assert isinstance(rounds, int)
    indexed: dict[
        tuple[str, str, int], dict[str, dict[str, object]]
    ] = {}
    records = campaign["records"]
    assert isinstance(records, list)
    for raw_record in records:
        if not isinstance(raw_record, dict):
            raise ValueError("campaign contains a non-object timing record")
        record = raw_record
        lane = record.get("lane")
        cell = record.get("cell")
        round_number = record.get("round")
        subject = record.get("subject")
        if lane not in lanes or cell not in cells:
            raise ValueError(f"timing record names unknown lane or cell: {record}")
        if type(round_number) is not int or not 1 <= round_number <= rounds:
            raise ValueError(f"invalid round in timing record: {record}")
        if subject not in SUBJECTS:
            raise ValueError(f"invalid subject in timing record: {record}")
        average_ns(record)
        key = (str(lane), str(cell), round_number)
        subjects = indexed.setdefault(key, {})
        if subject in subjects:
            raise ValueError(
                f"duplicate {subject} timing for lane={lane} cell={cell} "
                f"round={round_number}"
            )
        subjects[str(subject)] = record

    expected_rounds = set(range(1, rounds + 1))
    for lane in lanes:
        for cell in cells:
            seen_rounds = {
                round_number for (seen_lane, seen_cell, round_number), subjects
                in indexed.items()
                if seen_lane == lane and seen_cell == cell and
                set(subjects) == set(SUBJECTS)
            }
            if seen_rounds != expected_rounds:
                raise ValueError(
                    f"incomplete same-round subject pairing for lane={lane} "
                    f"cell={cell}; complete rounds={sorted(seen_rounds)}"
                )
    return indexed


def aggregate_bootstrap(
    cell_names: Sequence[str],
    cell_bootstraps: dict[str, list[float]],
    samples: int,
) -> list[float]:
    return [
        geometric_mean(cell_bootstraps[cell][sample] for cell in cell_names)
        for sample in range(samples)
    ]


def analyze_campaign(
    campaign: dict[str, object],
    campaign_path: pathlib.Path,
    bootstrap_samples: int = DEFAULT_BOOTSTRAP_SAMPLES,
    bootstrap_seed: int = DEFAULT_BOOTSTRAP_SEED,
    analyzer_path: Optional[pathlib.Path] = None,
) -> dict[str, object]:
    validate_campaign(campaign)
    indexed = index_records(campaign)
    lanes = campaign["lanes"]
    cells = campaign["cells"]
    rounds = campaign["rounds"]
    assert isinstance(lanes, dict)
    assert isinstance(cells, dict)
    assert isinstance(rounds, int)
    cell_names = list(cells)

    lane_analyses = []
    for lane_name, lane_metadata in lanes.items():
        cell_analyses = []
        observed_ratios: dict[str, dict[str, list[float]]] = {
            ratio_name: {} for ratio_name in RATIOS
        }
        bootstrap_medians: dict[str, dict[str, list[float]]] = {
            ratio_name: {} for ratio_name in RATIOS
        }
        for cell_name in cell_names:
            subject_measurements = {subject: [] for subject in SUBJECTS}
            round_ratios = {ratio_name: [] for ratio_name in RATIOS}
            for round_number in range(1, rounds + 1):
                records = indexed[(lane_name, cell_name, round_number)]
                measurements = {
                    subject: average_ns(records[subject])
                    for subject in SUBJECTS
                }
                for subject in SUBJECTS:
                    subject_measurements[subject].append(measurements[subject])
                for ratio_name, (numerator, denominator) in RATIOS.items():
                    round_ratios[ratio_name].append(
                        measurements[numerator] / measurements[denominator]
                    )

            cell_ratio_analysis = {}
            for ratio_name, values in round_ratios.items():
                observed_ratios[ratio_name][cell_name] = values
                bootstraps = bootstrap_cell_medians(
                    values,
                    bootstrap_samples,
                    stable_seed(
                        bootstrap_seed, lane_name, cell_name, ratio_name
                    ),
                )
                bootstrap_medians[ratio_name][cell_name] = bootstraps
                cell_ratio_analysis[ratio_name] = {
                    **ratio_summary(statistics.median(values), bootstraps),
                    "same_round_values": values,
                }

            metadata = cells[cell_name]
            if not isinstance(metadata, dict):
                raise ValueError(f"cell metadata is not an object: {cell_name}")
            cell_analyses.append({
                "name": cell_name,
                "category": category_for_cell(cell_name),
                "items": metadata.get("items"),
                "matches": metadata.get("matches"),
                "median_ns": {
                    subject: statistics.median(values)
                    for subject, values in subject_measurements.items()
                },
                "observed_average_ns": subject_measurements,
                "ratios": cell_ratio_analysis,
            })

        aggregate_ratios = {}
        for ratio_name in RATIOS:
            cell_estimates = {
                cell_name: statistics.median(
                    observed_ratios[ratio_name][cell_name]
                )
                for cell_name in cell_names
            }
            aggregate_draws = aggregate_bootstrap(
                cell_names,
                bootstrap_medians[ratio_name],
                bootstrap_samples,
            )
            aggregate_ratios[ratio_name] = {
                **ratio_summary(
                    geometric_mean(cell_estimates.values()), aggregate_draws
                ),
                "denominator_wins": sum(
                    estimate > 1.0 for estimate in cell_estimates.values()
                ),
                "ties": sum(
                    estimate == 1.0 for estimate in cell_estimates.values()
                ),
                "cell_count": len(cell_names),
            }

        categories = {}
        for category in CATEGORY_ORDER:
            category_cells = [
                cell_name for cell_name in cell_names
                if category_for_cell(cell_name) == category
            ]
            if not category_cells:
                categories[category] = {
                    "cells": [], "cell_count": 0, "ratios": None,
                }
                continue
            category_ratios = {}
            for ratio_name in RATIOS:
                estimates = [
                    statistics.median(
                        observed_ratios[ratio_name][cell_name]
                    )
                    for cell_name in category_cells
                ]
                draws = aggregate_bootstrap(
                    category_cells,
                    bootstrap_medians[ratio_name],
                    bootstrap_samples,
                )
                category_ratios[ratio_name] = ratio_summary(
                    geometric_mean(estimates), draws
                )
            categories[category] = {
                "cells": category_cells,
                "cell_count": len(category_cells),
                "ratios": category_ratios,
            }

        lane_analyses.append({
            "name": lane_name,
            "metadata": lane_metadata,
            "rounds": rounds,
            "cell_count": len(cell_names),
            "aggregate": {"ratios": aggregate_ratios},
            "categories": categories,
            "cells": cell_analyses,
        })

    result: dict[str, object] = {
        "schema": 1,
        "status": "complete",
        "campaign": str(campaign_path.resolve()),
        "campaign_sha256": sha256(campaign_path),
        "campaign_started_utc": campaign.get("started_utc"),
        "campaign_completed_utc": campaign.get("completed_utc"),
        "rounds": rounds,
        "fixed_cells": len(cell_names),
        "bootstrap": {
            "samples": bootstrap_samples,
            "seed": bootstrap_seed,
            "confidence_level": 0.95,
            "method": (
                "independently resample same-round ratios with replacement "
                "inside each fixed cell; take each cell median; take the "
                "equal-cell geometric mean"
            ),
            "percentile_interpolation": "linear",
        },
        "point_estimate": (
            "median same-round ratio per fixed cell, then equal-cell "
            "geometric mean"
        ),
        "ratio_semantics": {
            ratio_name: {
                "numerator": numerator,
                "denominator": denominator,
                "greater_than_one_favors": denominator,
            }
            for ratio_name, (numerator, denominator) in RATIOS.items()
        },
        "category_order": list(CATEGORY_ORDER),
        "lanes": lane_analyses,
    }
    if analyzer_path is not None:
        result["analyzer"] = str(analyzer_path.resolve())
        result["analyzer_sha256"] = sha256(analyzer_path)
    return result


def format_ratio(summary: dict[str, object], digits: int = 3) -> str:
    estimate = summary["estimate"]
    interval = summary["ci95"]
    assert isinstance(estimate, float)
    assert isinstance(interval, dict)
    return (
        f"{estimate:.{digits}f} "
        f"[{interval['lower']:.{digits}f}, {interval['upper']:.{digits}f}]"
    )


def render_markdown(analysis: dict[str, object]) -> str:
    lines = [
        "# fzf matched-envelope analysis",
        "",
        f"Campaign: `{analysis['campaign']}`",
        "",
        f"Campaign SHA-256: `{analysis['campaign_sha256']}`",
        "",
        (
            f"Each cell uses {analysis['rounds']} same-round process ratios. "
            f"Intervals use {analysis['bootstrap']['samples']:,} deterministic "
            "bootstrap samples."
        ),
        "",
        "Ratios greater than 1 favor the denominator named in the ratio.",
        "",
    ]
    lanes = analysis["lanes"]
    assert isinstance(lanes, list)
    for lane in lanes:
        assert isinstance(lane, dict)
        lines.extend((f"## {lane['name']}", "", "### Aggregate", ""))
        lines.extend((
            "| Ratio | Estimate and 95% interval | Denominator wins |",
            "|---|---:|---:|",
        ))
        aggregate = lane["aggregate"]
        assert isinstance(aggregate, dict)
        aggregate_ratios = aggregate["ratios"]
        assert isinstance(aggregate_ratios, dict)
        for ratio_name in RATIOS:
            summary = aggregate_ratios[ratio_name]
            lines.append(
                f"| {ratio_name} | {format_ratio(summary)} | "
                f"{summary['denominator_wins']}/{summary['cell_count']} |"
            )

        lines.extend(("", "### Categories", ""))
        lines.extend((
            "| Category | Cells | Base/candidate | Fzf/candidate | Fzf/base |",
            "|---|---:|---:|---:|---:|",
        ))
        categories = lane["categories"]
        assert isinstance(categories, dict)
        for category in CATEGORY_ORDER:
            details = categories[category]
            if details["ratios"] is None:
                continue
            ratio_details = details["ratios"]
            lines.append(
                f"| {category} | {details['cell_count']} | "
                f"{format_ratio(ratio_details['base/candidate'])} | "
                f"{format_ratio(ratio_details['fzf/candidate'])} | "
                f"{format_ratio(ratio_details['fzf/base'])} |"
            )

        lines.extend(("", "### Fixed cells", ""))
        lines.extend((
            "| Cell | Base ms | Candidate ms | Fzf ms | Base/candidate | "
            "Fzf/candidate | Fzf/base |",
            "|---|---:|---:|---:|---:|---:|---:|",
        ))
        cell_details = lane["cells"]
        assert isinstance(cell_details, list)
        for cell in cell_details:
            median_ns = cell["median_ns"]
            ratios = cell["ratios"]
            lines.append(
                f"| {cell['name']} | {median_ns['base'] / 1e6:.6f} | "
                f"{median_ns['candidate'] / 1e6:.6f} | "
                f"{median_ns['fzf'] / 1e6:.6f} | "
                f"{ratios['base/candidate']['estimate']:.3f} | "
                f"{ratios['fzf/candidate']['estimate']:.3f} | "
                f"{ratios['fzf/base']['estimate']:.3f} |"
            )
        lines.append("")
    return "\n".join(lines)


def render_tsv(analysis: dict[str, object]) -> str:
    output = io.StringIO()
    writer = csv.writer(output, delimiter="\t", lineterminator="\n")
    writer.writerow((
        "lane", "profile", "threads", "primary", "cell", "category",
        "base_median_ns", "candidate_median_ns", "fzf_median_ns",
        "base_candidate", "base_candidate_ci95_low",
        "base_candidate_ci95_high", "fzf_candidate",
        "fzf_candidate_ci95_low", "fzf_candidate_ci95_high", "fzf_base",
        "fzf_base_ci95_low", "fzf_base_ci95_high",
    ))
    lanes = analysis["lanes"]
    assert isinstance(lanes, list)
    for lane in lanes:
        metadata = lane["metadata"]
        for cell in lane["cells"]:
            row = [
                lane["name"], metadata.get("profile"), metadata.get("threads"),
                metadata.get("primary"), cell["name"], cell["category"],
                cell["median_ns"]["base"], cell["median_ns"]["candidate"],
                cell["median_ns"]["fzf"],
            ]
            for ratio_name in RATIOS:
                summary = cell["ratios"][ratio_name]
                row.extend((
                    summary["estimate"], summary["ci95"]["lower"],
                    summary["ci95"]["upper"],
                ))
            writer.writerow(row)
    return output.getvalue()


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("campaign", type=pathlib.Path)
    parser.add_argument(
        "--output-prefix", type=pathlib.Path,
        help=(
            "output path without extension (default: analysis beside campaign)"
        ),
    )
    parser.add_argument(
        "--bootstrap-samples", type=int, default=DEFAULT_BOOTSTRAP_SAMPLES
    )
    parser.add_argument(
        "--bootstrap-seed",
        type=lambda value: int(value, 0),
        default=DEFAULT_BOOTSTRAP_SEED,
    )
    arguments = parser.parse_args()
    campaign_path = arguments.campaign.resolve()
    campaign = json.loads(campaign_path.read_text(encoding="utf-8"))
    if not isinstance(campaign, dict):
        raise ValueError("campaign JSON must contain an object")
    script_path = pathlib.Path(__file__).resolve()
    analysis = analyze_campaign(
        campaign,
        campaign_path,
        bootstrap_samples=arguments.bootstrap_samples,
        bootstrap_seed=arguments.bootstrap_seed,
        analyzer_path=script_path,
    )

    prefix = arguments.output_prefix
    if prefix is None:
        prefix = campaign_path.with_name("analysis")
    prefix = prefix.resolve()
    prefix.parent.mkdir(parents=True, exist_ok=True)
    outputs = {
        ".json": json.dumps(analysis, indent=2, ensure_ascii=False) + "\n",
        ".md": render_markdown(analysis),
        ".tsv": render_tsv(analysis),
    }
    for suffix, contents in outputs.items():
        prefix.with_suffix(suffix).write_text(contents, encoding="utf-8")
    print(f"analysis-json {prefix.with_suffix('.json')}")
    print(f"analysis-markdown {prefix.with_suffix('.md')}")
    print(f"analysis-tsv {prefix.with_suffix('.tsv')}")


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"fzf envelope analysis: FAIL: {error}", file=sys.stderr)
        raise
