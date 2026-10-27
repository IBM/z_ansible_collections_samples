###############################################################################
# Copyright (c) IBM Corporation 2026
#
# Shared helpers for the zos_basics test suite.
# Imported by test_upgrade_validator.py and test_playbook_run.py.
###############################################################################

from __future__ import annotations

import os
import subprocess
from pathlib import Path

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
_TESTS_DIR = Path(__file__).parent.resolve()
_ZOS_BASICS_DIR = _TESTS_DIR.parent

# Explicit playbook entry-points per subfolder.
PLAYBOOKS_BY_SUBFOLDER: dict[str, list[str]] = {
    "constructs": [
        "site.yml",
        "zos_job_submit_ansible_constructs.yml",
    ],
    "factgathering": [
        "gather-facts.yml",
    ],
    "unarchive_pax_and_set_custom_facts": [
        "site.yml",
        "copy_sample_facts_files_to_remote_system.yml",
        "gather_and_print_facts.yml",
        "unarchive_pax_and_set_custom_facts.yml",
    ],
}


def ansible_env(ansible_cfg: Path | None = None) -> dict[str, str]:
    env = os.environ.copy()
    repo_root = _ZOS_BASICS_DIR.parent
    env["ANSIBLE_ROLES_PATH"] = str(repo_root / "roles")
    env["ANSIBLE_COLLECTIONS_PATH"] = str(Path.home() / ".ansible" / "collections")
    env.pop("ANSIBLE_COLLECTIONS_PATHS", None)
    if ansible_cfg is not None:
        env["ANSIBLE_CONFIG"] = str(ansible_cfg)
    return env


def collection_installed() -> bool:
    """Return True if ibm.ibm_zos_core (>= 2.0.0) is installed.

    Shells out to ``ansible-galaxy collection list ibm.ibm_zos_core``.
    Returns False when ansible-galaxy is not on PATH.
    """
    try:
        result = subprocess.run(
            ["ansible-galaxy", "collection", "list", "ibm.ibm_zos_core"],
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        return False

    for line in result.stdout.splitlines():
        if line.strip().startswith("ibm.ibm_zos_core"):
            return True
    return False
