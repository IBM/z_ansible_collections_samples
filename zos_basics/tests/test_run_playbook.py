# Copyright (c) IBM Corporation 2026
#
# Tests that run every zos_basics playbook against a real z/OS host and assert
# a zero exit code.
#
# Requirements
# ------------
# - ibm.ibm_zos_core collection (>= 2.0.0) must be installed locally.
# - Update the inventory files with your z/OS system configuration variables.
#
# Run pytest
# ------------
# pytest zos_basics/tests/test_run_playbook.py
###############################################################################

from pathlib import Path
import pytest
import subprocess

from helpers import (
    ansible_env,
    collection_installed,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
_TEST_DIR = Path(__file__).parent.resolve()
_ZOS_BASICS_DIR = _TEST_DIR.parent

_INVENTORIES_DEFAULT = _TEST_DIR / "inventories"

_SITE_PLAYBOOKS: set[Path] = {
    _ZOS_BASICS_DIR / "constructs" / "site.yml",
    _ZOS_BASICS_DIR / "factgathering" / "site.yml",
    # _ZOS_BASICS_DIR / "unarchive_pax_and_set_custom_facts" / "site.yml",
}

# ---------------------------------------------------------------------------
# Session-scoped fixures
# ---------------------------------------------------------------------------
@pytest.fixture(scope="session", autouse=True)
def require_collection():
    # Skip when the collection is not installed (site.yml uses requirements-check role).
    if not collection_installed():
        pytest.skip(
            "ibm.ibm_zos_core not installed; skipping all site.yml playbook tests.",
            allow_module_level=True,
        )

# ---------------------------------------------------------------------------
# Test
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "playbook_path",
    sorted(_SITE_PLAYBOOKS),
    ids=[str(p.relative_to(_ZOS_BASICS_DIR)) for p in sorted(_SITE_PLAYBOOKS)],
)
def test_playbook_run(playbook_path: Path) -> None:
    """Run a site.yml playbook against a real z/OS host and assert exit code 0."""

    # Set inventory
    inventory = _INVENTORIES_DEFAULT

    # Build playbook run command
    command = ["ansible-playbook", "-i", inventory, str(playbook_path)]

    # Custom commands
    if playbook_path == _ZOS_BASICS_DIR / "constructs" / "site.yml":
        command.extend(["--tags", "mode_production"])

    # Set ansible configuration file
    cfg_path = playbook_path.parent / "ansible.cfg"
    if not cfg_path.exists():
        cfg_path = None

    result = subprocess.run(
        command,
        capture_output=True,
        text=True,
        cwd=str(playbook_path.parent),
        env=ansible_env(ansible_cfg=cfg_path),
    )

    assert result.returncode == 0, (
        f"ansible-playbook exited {result.returncode} for {playbook_path.name}.\n"
        f"STDOUT:\n{result.stdout}\n"
        f"STDERR:\n{result.stderr}"
    )