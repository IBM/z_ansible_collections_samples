###############################################################################
# Copyright (c) IBM Corporation 2026
#
# Shared pytest fixtures for the zos_basics test suite
###############################################################################

from __future__ import annotations
from pathlib import Path
import pytest

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------
@pytest.fixture
def migration_reports_dir(tmp_path: Path) -> Path:
    """Return a temporary directory for upgrade-validator JSON reports."""
    reports = tmp_path / "migration_reports"
    reports.mkdir()
    return reports
