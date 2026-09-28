###############################################################################
# Copyright (c) IBM Corporation 2026
#
# Tests that run the ibm.ibm_zos_core.playbook_upgrade_validator role against
# every zos_concepts playbook and assert the resulting JSON migration report
# contains no [MUST_FIX] violations.
###############################################################################

from pathlib import Path
from helpers import collection_installed, ansible_env, PLAYBOOKS_BY_SUBFOLDER
import textwrap
import pytest
import warnings
import subprocess
from typing import Any
import json


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
_TEST_DIR = Path(__file__).parent.resolve()
_ZOS_CONCEPTS_DIR = _TEST_DIR.parent

# Flat list of (subfolder_name, playbook_path) pairs used to parametrize.
_PLAYBOOK_PARAMS: list[tuple[str, Path]] = [
    (subfolder, _ZOS_CONCEPTS_DIR / subfolder / playbook)
    for subfolder, playbooks in PLAYBOOKS_BY_SUBFOLDER.items()
    for playbook in playbooks
]

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
def _validator_playbook_content(playbook_path: Path, report_path: Path) -> str:
    """Return the YAML content for the migration validator for a playbook."""
    return textwrap.dedent(
        f"""\
        - name: Validate playbook for migration to ibm_zos_core v2.0.0
          hosts: localhost
          gather_facts: false
          roles: 
              - role: ibm.ibm_zos_core.playbook_upgrade_validator
                vars:
                  playbook_upgrade_validator_playbook_path: "{playbook_path}"
                  playbook_upgrade_validator_output_path: "{report_path}"
                  playbook_upgrade_validator_ignore_response_params: false
        """
    )

def _read_violations(report_path: Path) -> list[dict[str, Any]]:
    """Parse the migration report JSON and return any violation entries."""
    if not report_path.exists():
        return []

    try:
        data = json.loads(report_path.read_text())
    except json.JSONDecodeError as exception:
        pytest.fail(f"Could not parse migration report {report_path}: {exception}")

    violations: list[dict[str, Any]] = []
    entries = data.get("migration_report", data) if isinstance(data, dict) else data
    if not isinstance(entries, list):
        return violations
    
    for task_entry in entries:
        for action in task_entry.get("migration_actions", []):
            if "[MUST_FIX]" not in action:
                continue
            violations.append(
                {
                    "playbook": task_entry.get("playbook", ""),
                    "task": task_entry.get("task_name", ""),
                    "module": task_entry.get("module", ""),
                    "message": action,
                }
            )
    return violations
        

# ---------------------------------------------------------------------------
# Test
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "subfolder,playbook_path",
    _PLAYBOOK_PARAMS,
    ids=[f"{subfolder}/{playbook}" for subfolder, playbook in [(sf, pb.name) for sf, pb in _PLAYBOOK_PARAMS]],
)
def test_upgrade_validator(
    subfolder: str,
    playbook_path: Path,
    migration_reports_dir: Path,
    tmp_path: Path,
) -> None:
    """Run playbook_upgrade_validator and assert the report has no violations."""

    if not collection_installed():
        pytest.skip(
            "ibm.ibm_zos_core collection not found — install it with "
            "'ansible-galaxy collection install ibm.ibm_zos_core' to run this test"
        )

    # Set up migration report file and upgrade validator playbook
    report_path = migration_reports_dir / f"migration_report_{playbook_path.stem}.json"
    validator_playbook_path = tmp_path / f"validator_playbook_{playbook_path.stem}.yml"
    validator_playbook_path.write_text(_validator_playbook_content(playbook_path, report_path))

    result = subprocess.run(
        ["ansible-playbook", str(validator_playbook_path)],
        capture_output=True,
        text=True,
        env=ansible_env(),
    )

    if result.returncode != 0:
        pytest.fail(
            f"ansible-playbook wrapper exited {result.returncode} for "
            f"{playbook_path.name}.\n"
            f"STDOUT:\n{result.stdout}\n"
            f"STDERR:\n{result.stderr}"
        )
    
    if not report_path.exists():
        warnings.warn(
            f"Migration report was not generated for {playbook_path.name}. "
            "The validator may not have found any ibm_zos_core module calls.",
            UserWarning,
            stacklevel=2,
        )
        return
    
    violations = _read_violations(report_path)

    if violations:
        violation_text = "\n".join(
            [f"[{i+1}] playbook={v['playbook']}\n"
             f"task={v['task']}\n"
             f" module={v['module']}\n"
             f"{v['message']}"
             for i, v in enumerate(violations)]
        )
        pytest.fail(
            f"{len(violations)} violation(s) found in {playbook_path.name}:\n"
            f"{violation_text}"
        )
