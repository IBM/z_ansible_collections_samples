# Copyright (c) IBM Corporation 2026
#
# Tests that run every zos_concepts playbook against a real z/OS host and assert
# a zero exit code.
#
# Requirements
# ------------
# - ibm.ibm_zos_core collection (>= 2.0.0) must be installed locally.
#
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
_ZOS_CONCEPTS_DIR = _TEST_DIR.parent

# site.yml playbooks include the requirements-check role; skip them when the
# collection is not installed.
_SITE_PLAYBOOKS = {
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
    _ZOS_CONCEPTS_DIR / "jobs" / "submit_multiple_jobs_async" / "site.yml",
    _ZOS_CONCEPTS_DIR / "jobs" / "submit_query_retrieve" / "site.yml",
    _ZOS_CONCEPTS_DIR / "manipulate_text" / "site.yml",
    _ZOS_CONCEPTS_DIR / "program_authorization" / "git_apf" / "site.yml",
    _ZOS_CONCEPTS_DIR / "rest_apis" / "site.yml",
    _ZOS_CONCEPTS_DIR / "software_management" / "site.yml",
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
# Test
# ---------------------------------------------------------------------------
@pytest.mark.parametrize(
    "playbook_path",
    sorted(_SITE_PLAYBOOKS),
    ids=[str(p.relative_to(_ZOS_CONCEPTS_DIR)) for p in sorted(_SITE_PLAYBOOKS)],
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
    if (_ZOS_CONCEPTS_DIR / "data_transfer" / "archive_copy_unarchive_restore") in playbook_path.parents:
        inventory = _TEST_DIR / "inventories_data_transfer"
    elif (_ZOS_CONCEPTS_DIR / "data_transfer" / "dump_pack_ftp_unpack_restore") in playbook_path.parents:
        inventory = _TEST_DIR / "inventories_data_transfer"
    else:
        inventory = _TEST_DIR / "inventories"

    # Build playbook run command
    command = ["ansible-playbook", "-i", inventory, str(playbook_path)]

    # Custom commands
    if playbook_path == _ZOS_CONCEPTS_DIR / "zos_script" / "site.yml":
        command.extend(["-e", "python_script_dir=/u/omvsadm"])
    elif playbook_path == _ZOS_CONCEPTS_DIR / "zos_stat" / "site.yml":
        jcl_file = _ZOS_CONCEPTS_DIR / "zos_stat" / "files" / "HELLO.jcl"
        command.extend(["-e", f"jcl_file={jcl_file}"])
    elif playbook_path == _ZOS_CONCEPTS_DIR / "volume_management" / "volume_initialization" / "init_dasd_vol_and_run_sample_jcl" / "site.yml":
        command.extend(["-e", '{"vol_unit": "01A0", "new_volser": "ABC123"}'])
    elif playbook_path == _ZOS_CONCEPTS_DIR / "user_management" / "add_remove_user" / "site.yml":
        command.extend(["-e", '{"name": "testusr", "userid": "TESTU8", "user_catalog": "VCATQAV"}'])

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