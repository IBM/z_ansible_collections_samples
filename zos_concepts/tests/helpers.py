###############################################################################
# Copyright (c) IBM Corporation 2026
#
# Shared helpers for the zos_concepts test suite.
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
_ZOS_CONCEPTS_DIR = _TESTS_DIR.parent

# Explicit playbook entry-points per subfolder.
PLAYBOOKS_BY_SUBFOLDER: dict[str, list[str]] = {
    "certificate_management": [
        "create_cert.yml",
        "create_keyring.yml",
        "delete_cert.yml",
        "delete_keyring.yml",
        "health_checker_security.yml",
        "list_cert.yml",
        "search_and_renew.yml",
    ],
    "data_sets/copy_edit_submit": [
        "site.yml",
        "copy_edit_submit.yml",
    ],
    "data_sets/data_set_basics": [
        "site.yml",
        "data_set_basics.yml",
    ],
    "data_transfer/archive_copy_unarchive_restore": [
        "site.yml",
        "archive_fetch_data_sets.yml",
        "unarchive_data_sets.yml",
    ],
    "data_transfer/copy_fetch_data_set": [
        "site.yml",
        "copy_fetch_data_set.yml",
    ],
    "data_transfer/copy_sort_fetch": [
        "site.yml",
        "copy-sort-fetch.yml",
    ],
    "data_transfer/dump_pack_ftp_unpack_restore": [
        "site.yml",
        "dump-pack-ftp.yml",
        "transfer-data-sets.yml",
        "unpack-restore.yml",
    ],
    "data_transfer/terse_fetch_data_set": [
        "site.yml",
        "terse_fetch_data_set.yml",
    ],
    "encoding/convert_encoding": [
        "site.yml",
        "convert_encoding.yml",
    ],
    "gdg_datasets/copy_edit_fetch": [
        "site.yml",
        "copy_edit_fetch.yml",
    ],
    "gdg_datasets/create_copy_submit": [
        "site.yml",
        "create_copy_submit.yml",
    ],
    "jobs/submit_multiple_jobs_async": [
        "site.yml",
        "submit_async_query_retrieve.yml",
    ],
    "jobs/submit_query_retrieve": [
        "site.yml",
        "submit_query_retrieve.yml",
    ],
    "manipulate_text": [
        "site.yml",
        "manipulate_text.yml",
    ],
    "program_authorization/git_apf": [
        "site.yml",
        "prog_auth.yml",
    ],
    "rest_apis": [
        "site.yml",
        "uri-sample.yml",
    ],
    "software_management": [
        "site.yml",
        "accept.yml",
        "install.yml",
        "order.yml",
        "query_csi.yml",
        "reject.yml",
        "setup_internet_retrieval.yml",
        "uninstall.yml",
    ],
    "templates/copy_template": [
        "site.yml",
        "zos_copy_template.yml",
    ],
    "templates/loadlib": [
        "site.yml",
        "compile_link_loadlib.yml",
    ],
    "templates/submit_job_template": [
        "site.yml",
        "submit_templates.yml",
    ],
    "tso_commands/scripts": [
        "site.yml",
        "run_rexx_and_clist.yml",
    ],
    "user_management/add_remove_user": [
        "site.yml",
        "add-user.yml",
        "remove-user.yml",
        "send-rejection-email.yml",
    ],
    "user_management/zos_user": [
        "site.yml",
        "zos_user.yml",
    ],
    "volume_management/volume_initialization/init_dasd_vol_and_run_sample_jcl": [
        "site.yml",
        "init_dasd_vol_and_run_sample_jcl.yml",
    ],
    "zfsadm/grow_zfs_fetch_trace_back": [
        "site.yml",
        "grow_zfs_fetch_trace_back.yml",
    ],
    "zfsadm/shrink_zfs_different_size_and_verbose": [
        "site.yml",
        "shrink_zfs_different_size_and_verbose.yml",
    ],
    "zfsadm/zfs_grow_aggr": [
        "site.yml",
        "grow_zfs_aggregate.yml",
        "scan_and_grow_zfs_aggr.yml",
    ],
    "zos_operator/zos_operator_basics": [
        "site.yml",
        "zos_operator_basics.yml",
    ],
    "zos_ping": [
        "site.yml",
        "zos_ping.yaml",
    ],
    "zos_script": [
        "site.yml",
        "zos_script.yml",
    ],
    "zos_started_task": [
        "site.yml",
        "zos_started_task.yml",
    ],
    "zos_stat": [
        "site.yml",
        "zos_stat.yml",
    ],
}

def ansible_env(ansible_cfg: Path | None = None) -> dict[str, str]:
    env = os.environ.copy()
    repo_root = _ZOS_CONCEPTS_DIR.parent
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
