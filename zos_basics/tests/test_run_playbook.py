# Copyright (c) IBM Corporation 2026
#
# Tests that run every zos_basics playbook against a real z/OS host and assert
# a zero exit code.
#
# Requirements
# ------------
# - Set the ZOS_INVENTORY environment variable to the path of a valid Ansible
#   inventory before running these tests.  Example:
#
#     ZOS_INVENTORY=/path/to/inventories pytest zos_basics/tests/test_playbook_run.py
#
# - ibm.ibm_zos_core collection (>= 2.0.0) must be installed locally.
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

# site.yml playbooks include the requirements-check role; skip them when the
# collection is not installed.
_SITE_PLAYBOOKS = {
    _ZOS_BASICS_DIR / "constructs" / "site.yml",
    _ZOS_BASICS_DIR / "unarchive_pax_and_set_custom_facts" / "site.yml",
}

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

    # Skip when the collection is not installed (site.yml uses requirements-check role).
    if not collection_installed():
        pytest.skip(
            f"{playbook_path.name} uses the requirements-check role which "
            "depends on ibm.ibm_zos_core. Install the collection to run this test."
        )

    # Set inventory
    inventory = _TEST_DIR / "inventories"

    command = ["ansible-playbook", "-i", inventory, str(playbook_path)]

    # Custom commands
    if playbook_path == _ZOS_BASICS_DIR / "constructs" / "zos_job_submit_ansible_constructs.yml":
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