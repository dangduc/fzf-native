#!/usr/bin/env python3
"""Regression tests for analyze-fzf-envelope.py."""

from __future__ import annotations

import copy
import importlib.util
import json
import math
import pathlib
import tempfile
import unittest


SCRIPT = pathlib.Path(__file__).with_name("analyze-fzf-envelope.py")
SPEC = importlib.util.spec_from_file_location("analyze_fzf_envelope", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
ANALYZER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ANALYZER)


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
CELLS = ("chromium", "partial-16", "all-16", "partial-miss-16", "none-16")


def make_campaign() -> dict[str, object]:
    records = []
    # These pairs prove that the analyzer uses same-round ratios.  For
    # base/candidate, median([2 / 1, 300 / 100]) is 2.5.  Dividing the two
    # subject medians instead would give 151 / 50.5, about 2.99.
    values = {
        1: {"candidate": 1, "base": 2, "fzf": 8},
        2: {"candidate": 100, "base": 300, "fzf": 400},
    }
    for lane in LANES:
        for cell in CELLS:
            for round_number in (1, 2):
                for subject, total_ns in values[round_number].items():
                    records.append({
                        "lane": lane,
                        "cell": cell,
                        "round": round_number,
                        "subject": subject,
                        "iterations": 1,
                        "total_ns": total_ns,
                        "average_ns": float(total_ns),
                    })
    return {
        "schema": 1,
        "status": "ordered-output-and-timings-complete",
        "started_utc": "2026-09-09T00:00:00Z",
        "completed_utc": "2026-09-09T01:00:00Z",
        "rounds": 2,
        "lanes": LANES,
        "cells": {
            cell: {"items": 100, "matches": 10, "query": "q"}
            for cell in CELLS
        },
        "record_count": len(records),
        "records": records,
    }


class AnalyzerTest(unittest.TestCase):
    def analyze(self, campaign: dict[str, object]) -> dict[str, object]:
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "campaign.json"
            path.write_text(json.dumps(campaign), encoding="utf-8")
            return ANALYZER.analyze_campaign(
                campaign,
                path,
                bootstrap_samples=101,
                bootstrap_seed=12345,
                analyzer_path=SCRIPT,
            )

    def test_same_round_ratios_categories_and_all_lanes(self) -> None:
        analysis = self.analyze(make_campaign())
        self.assertEqual(len(analysis["lanes"]), 4)
        for lane in analysis["lanes"]:
            ratios = lane["aggregate"]["ratios"]
            self.assertTrue(math.isclose(
                ratios["base/candidate"]["estimate"], 2.5
            ))
            self.assertTrue(math.isclose(
                ratios["fzf/candidate"]["estimate"], 6.0
            ))
            self.assertTrue(math.isclose(
                ratios["fzf/base"]["estimate"], 8.0 / 3.0
            ))
            self.assertEqual(list(lane["categories"]), list(ANALYZER.CATEGORY_ORDER))
            self.assertTrue(all(
                details["cell_count"] == 1
                for details in lane["categories"].values()
            ))

    def test_bootstrap_is_deterministic(self) -> None:
        campaign = make_campaign()
        first = self.analyze(campaign)
        second = self.analyze(campaign)
        self.assertEqual(first["lanes"], second["lanes"])

    def test_missing_same_round_subject_is_rejected(self) -> None:
        campaign = make_campaign()
        campaign["records"].pop()
        campaign["record_count"] -= 1
        with self.assertRaisesRegex(ValueError, "expected"):
            self.analyze(campaign)

    def test_rounded_average_is_rejected(self) -> None:
        campaign = copy.deepcopy(make_campaign())
        campaign["records"][0]["iterations"] = 3
        campaign["records"][0]["total_ns"] = 10
        campaign["records"][0]["average_ns"] = 3.33
        with self.assertRaisesRegex(ValueError, "differs"):
            self.analyze(campaign)


if __name__ == "__main__":
    unittest.main()
