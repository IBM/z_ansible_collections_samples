# Copyright (c) IBM Corporation 2026
#
# Tests that run every zos_concepts playbook against a real z/OS host and assert
# a zero exit code.
#
# Requirements
# ------------
# - ibm.ibm_zos_core collection (>= 2.0.0) must be installed locally.
# - Supply inventory files with your z/OS system configuration variables to
#   _INVENTORIES_DEFAULT and _INVENTORIES_DATA_TRANSFER variables.
#
# Run pytest
# ------------
# pytest zos_concepts/tests/test_run_playbook.py
###############################################################################

from pathlib import Path
import pytest
import subprocess
import tempfile

from helpers import (
    ansible_env,
    collection_installed,
)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
_TEST_DIR = Path(__file__).parent.resolve()
_ZOS_CONCEPTS_DIR = _TEST_DIR.parent

_FILE_GENERATION_PLAYBOOKS =  {
    _ZOS_CONCEPTS_DIR / "data_transfer" / "copy_sort_fetch" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_transfer" / "terse_fetch_data_set" / "site.yml",
    _ZOS_CONCEPTS_DIR / "gdg_datasets" / "copy_edit_fetch" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zfsadm" / "grow_zfs_fetch_trace_back" / "site.yml",
}

_INVENTORIES_DEFAULT = _TEST_DIR / "inventories"
# Data transfer playbooks use different inventory file variables
_INVENTORIES_DATA_TRANSFER = _TEST_DIR / "inventories_data_transfer"
_INVENTORY_OVERRIDES: dict[Path, Path] = {
    _ZOS_CONCEPTS_DIR / "data_transfer" / "archive_copy_unarchive_restore": _INVENTORIES_DATA_TRANSFER,
    _ZOS_CONCEPTS_DIR / "data_transfer" / "dump_pack_ftp_unpack_restore":    _INVENTORIES_DATA_TRANSFER,
}

_EXTRA_VARS: dict[Path, str] = {
    _ZOS_CONCEPTS_DIR / "zos_script" / "site.yml":
        "python_script_dir=/u/omvsadm",
    _ZOS_CONCEPTS_DIR / "zos_stat" / "site.yml":
        f"jcl_file={_ZOS_CONCEPTS_DIR / 'zos_stat' / 'files' / 'HELLO.jcl'}",
    _ZOS_CONCEPTS_DIR / "volume_management" / "volume_initialization" / "init_dasd_vol_and_run_sample_jcl" / "site.yml":
        '{"vol_unit": "01A0", "new_volser": "ABC123"}',
    _ZOS_CONCEPTS_DIR / "user_management" / "add_remove_user" / "site.yml":
        '{"name": "testusr", "userid": "TESTU8", "user_catalog": "VCATQAV"}',
}

_SITE_PLAYBOOKS: set[Path] = {
    _ZOS_CONCEPTS_DIR / "data_sets" / "copy_edit_submit" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_sets" / "data_set_basics" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_transfer" / "archive_copy_unarchive_restore" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_transfer" / "copy_fetch_data_set" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_transfer" / "copy_sort_fetch" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_transfer" / "dump_pack_ftp_unpack_restore" / "site.yml",
    _ZOS_CONCEPTS_DIR / "data_transfer" / "terse_fetch_data_set" / "site.yml",
    _ZOS_CONCEPTS_DIR / "encoding" / "convert_encoding" / "site.yml",
    _ZOS_CONCEPTS_DIR / "gdg_datasets" / "copy_edit_fetch" / "site.yml",
    _ZOS_CONCEPTS_DIR / "gdg_datasets" / "create_copy_submit" / "site.yml",
    _ZOS_CONCEPTS_DIR / "jobs" / "submit_query_retrieve" / "site.yml",
    _ZOS_CONCEPTS_DIR / "jobs" / "submit_multiple_jobs_async" / "site.yml",
    _ZOS_CONCEPTS_DIR / "manipulate_text" / "site.yml",
    _ZOS_CONCEPTS_DIR / "program_authorization" / "git_apf" / "site.yml",
    # _ZOS_CONCEPTS_DIR / "rest_apis" / "site.yml",
    # _ZOS_CONCEPTS_DIR / "software_management" / "site.yml",
    _ZOS_CONCEPTS_DIR / "templates" / "copy_template" / "site.yml",
    _ZOS_CONCEPTS_DIR / "templates" / "loadlib" / "site.yml",
    _ZOS_CONCEPTS_DIR / "templates" / "submit_job_template" / "site.yml",
    _ZOS_CONCEPTS_DIR / "tso_commands" / "scripts" / "site.yml",
    _ZOS_CONCEPTS_DIR / "user_management" / "add_remove_user" / "site.yml",
    _ZOS_CONCEPTS_DIR / "user_management" / "zos_user" / "site.yml",
    _ZOS_CONCEPTS_DIR / "volume_management" / "volume_initialization" / "init_dasd_vol_and_run_sample_jcl" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zfsadm" / "grow_zfs_fetch_trace_back" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zfsadm" / "shrink_zfs_different_size_and_verbose" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zfsadm" / "zfs_grow_aggr" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zos_operator" / "zos_operator_basics" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zos_ping" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zos_script" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zos_started_task" / "site.yml",
    _ZOS_CONCEPTS_DIR / "zos_stat" / "site.yml",
}

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------
def _resolve_inventory(playbook_path: Path) -> Path:
    for prefix, inventory in _INVENTORY_OVERRIDES.items():
        if prefix in playbook_path.parents:
            return inventory
    return _INVENTORIES_DEFAULT

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

# Create temporary directory for generated files
@pytest.fixture(scope="session", autouse=True)
def files_dir():
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)

# ---------------------------------------------------------------------------
# Test
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "playbook_path",
    sorted(_SITE_PLAYBOOKS),
    ids=[str(p.relative_to(_ZOS_CONCEPTS_DIR)) for p in sorted(_SITE_PLAYBOOKS)],
)
def test_playbook_run(playbook_path: Path, files_dir: Path) -> None:
    """Run a site.yml playbook against a real z/OS host and assert exit code 0."""

    # Set inventory
    inventory = _resolve_inventory(playbook_path)

    # Build playbook run command
    command = ["ansible-playbook", "-i", inventory, str(playbook_path)]

    # Set custom variables for playbook run
    if extra_vars := _EXTRA_VARS.get(playbook_path):
        command.extend(["-e", extra_vars])

    # Set temporary files directory
    output_path = files_dir
    if playbook_path in _FILE_GENERATION_PLAYBOOKS:
        command.extend(["-e", f"output_path={output_path}"])
        
    # Set ansible configuration file
    cfg_path = playbook_path.parent / "ansible.cfg"
    if not cfg_path.exists():
        cfg_path = None

    # Run playbook command
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