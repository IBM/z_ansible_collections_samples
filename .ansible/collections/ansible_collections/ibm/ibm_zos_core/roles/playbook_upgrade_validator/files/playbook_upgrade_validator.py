# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2026
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
from __future__ import absolute_import, division, print_function
__metaclass__ = type

DOCUMENTATION = r'''
---
module: playbook_upgrade_validator
version_added: "2.0.0"
author:
  - "Ravella Surendra Babu (@surendrababuravella)"
short_description: Build a report to migrate playbooks to v2.0.0
description:
  - Scans one or more Ansible playbooks to identify removed or renamed parameters
    based on migration rules for IBM z/OS Core collection version 2.0.0.
  - Provides line numbers, affected modules, and suggested corrective actions.
options:
  playbook_upgrade_validator_ignore_response_params:
    description:
      - Indicates whether information about response parameter changes should be included.
    required: false
    type: bool
  playbook_upgrade_validator_migration_map:
    description:
      - A structured set of migration rules that specifies removed, renamed, and modified parameters
        to help upgrade playbooks from ibm_zos_core 1.x.x to 2.0.0.
    required: true
    type: dict
  playbook_upgrade_validator_output_path:
    description:
      - Path to the output JSON file where results should be saved.
      - Default path is <<playbook_dir>>/logs/migration_report.json
    required: true
    type: str
  playbook_upgrade_validator_playbook_path:
    description:
      - Path to a single Ansible playbook file or a directory containing playbooks to be validated.
      - If a directory is provided, all .yml and .yaml files will be recursively scanned.
    required: true
    type: str
notes:
  - Designed to assist migration of playbooks from older IBM z/OS core collection versions to 2.0.0.
  - Supports reading tasks, blocks, and nested includes.
  - Reported task line numbers rely on task names and may be ambiguous when duplicate task names are used within a playbook.
'''

EXAMPLES = r'''
- name: execute playbook_upgrade_validator role to list migration changes
  include_role:
    name: ibm.ibm_zos_core.playbook_upgrade_validator
  vars:
    playbook_upgrade_validator_playbook_path: "/path/to/playbooks/*.yml"
    playbook_upgrade_validator_output_path: "/path/to/reports/validation_report.json"
    playbook_upgrade_validator_ignore_response_params: false
'''

RETURN = r'''
changed:
  description:
    - Always false as there are no state changes happening in this process.
  returned: always
  type: bool
playbook_upgrade_validator_output_path:
  description: Path to the output JSON file containing validation results.
  returned: always
  type: str
playbook_upgrade_validator_playbook_path:
  description: The path to the directory containing the Ansible playbooks to be validated.
  returned: always
  type: str
results:
  description: List of issues identified in all scanned playbooks, along with detailed information.
  returned: always
  type: list
  sample:
    [
        {
            "task_line": 9,
            "migration_actions": [
                "[MUST_FIX] Param 'force_lock' is renamed to 'force' in zos_copy",
                "[MUST_FIX] Param 'is_binary' is renamed to 'binary' in zos_copy",
                "[MUST_FIX] Param 'force' is renamed to 'replace' in zos_copy"
            ],
            "module": "zos_copy",
            "play_name": "Execute z/OS modules",
            "playbook": "/path/to/playbook/copy_file.yml",
            "task_name": "copy file to z/os"
        }
    ]
'''

import os
import argparse
import json
import sys
import yaml
import re


class LineNumberTracker:
    """Helper class to track line numbers in YAML files."""

    def __init__(self, filepath):
        """Initialize with file content."""
        with open(filepath, 'r') as f:
            self.lines = f.readlines()
        self.line_map = {}
        self._build_line_map()

    def _build_line_map(self):
        """Build a map of task names to line numbers."""
        for i, line in enumerate(self.lines):
            # Look for task names
            match = re.search(r'name:\s*["\']?([^"\']+)["\']?', line)
            if match:
                task_name = match.group(1).strip()
                self.line_map[task_name] = i + 1

            # Look for module names (for tasks without names)
            match = re.search(r'^\s*-?\s*([a-z_]+\.[a-z_]+\.[a-z_]+):', line)
            if match:
                module_name = match.group(1)
                self.line_map[f"_module_{module_name}"] = i + 1

    def get_line_number(self, task_name=None, module_name=None):
        """Get line number for a task or module."""
        if task_name and task_name in self.line_map:
            return self.line_map[task_name]
        if module_name:
            key = f"_module_{module_name}"
            if key in self.line_map:
                return self.line_map[key]
        return None


def load_playbook(path):
    """Load playbook YAML using PyYAML with support for custom tags."""
    # Register constructors for common custom YAML tags
    def generic_constructor(loader, tag_suffix, node):
        """Generic constructor that preserves the tag and value as a string."""
        if isinstance(node, yaml.ScalarNode):
            return f"{tag_suffix} {loader.construct_scalar(node)}"
        elif isinstance(node, yaml.SequenceNode):
            return loader.construct_sequence(node)
        elif isinstance(node, yaml.MappingNode):
            return loader.construct_mapping(node)
        return loader.construct_object(node)

    # Register multi-constructor to handle any custom tag (!, !var, !vault, etc.)
    yaml.add_multi_constructor('!', generic_constructor, Loader=yaml.SafeLoader)

    try:
        with open(path, "r") as f:
            data = yaml.safe_load(f)
            if isinstance(data, dict):
                data = [data]
            return data
    except yaml.YAMLError as e:
        raise ValueError(f"Error parsing YAML file {path}: {e}")
    except Exception as e:
        raise ValueError(f"Error reading file {path}: {e}")


def walk_tasks(tasks, play_name, line_tracker=None):
    """Recursively walk through tasks including block/rescue/always."""
    if not isinstance(tasks, list):
        return
    for task in tasks:
        if not isinstance(task, dict):
            continue
        task_name = task.get("name", "unknown")

        # Identify the module name (skip non-module keys)
        module_name = next(
            (
                k for k in task.keys()
                if k not in [
                    "name", "register", "vars", "when", "tags", "args",
                    "block", "rescue", "always", "delegate_to", "environment",
                    "become", "notify", "loop", "with_items", "with_dict",
                    "ignore_errors", "changed_when", "failed_when", "until",
                    "retries", "delay", "no_log", "run_once", "check_mode",
                    "diff", "any_errors_fatal", "connection", "port", "remote_user",
                    "become_user", "become_method", "become_flags", "debugger"
                ]
            ),
            None
        )

        if module_name:
            # Get params from module key or args key
            params = task.get(module_name, {})

            # Try to get line number
            line_num = None
            if line_tracker:
                line_num = line_tracker.get_line_number(
                    task_name=task_name if task_name != "unknown" else None,
                    module_name=module_name
                )

            yield {
                "play_name": play_name,
                "task_name": task_name,
                "module": module_name,
                "params": params,
                "task_line": line_num
            }

        # Recursive sections
        for section in ["block", "rescue", "always"]:
            if section in task:
                yield from walk_tasks(task[section], play_name, line_tracker)


def has_nested_key(data, dotted_key):
    """Check for nested key like param.subparam.name."""
    if not isinstance(data, dict):
        return False
    value = data
    for part in dotted_key.split("."):
        if isinstance(value, dict) and part in value:
            value = value[part]
        else:
            return False
    return True


def get_tasks_from_playbook(playbook_path, line_tracker=None):
    """Parse a playbook and extract module data."""
    playbook = load_playbook(playbook_path)
    if not isinstance(playbook, list):
        return []

    for play in playbook:
        if not isinstance(play, dict):
            continue

        # Check if this is a play (has 'hosts' or 'tasks' key) or a direct task
        if "hosts" in play or "tasks" in play:
            # This is a proper play structure
            play_name = play.get("name", "unknown")
            yield from walk_tasks(play.get("tasks", []), play_name, line_tracker)
        else:
            # This might be a direct task (taskfile format)
            # Treat the entire list as tasks
            play_name = "unknown"
            # Process this item as a task and break to avoid double-processing
            yield from walk_tasks([play], play_name, line_tracker)


def validate_tasks(playbook_path, migration_map, ignore_response_params):
    """Check tasks for removed or renamed params."""
    results = []

    # Create line tracker for this file
    try:
        line_tracker = LineNumberTracker(playbook_path)
    except Exception:
        line_tracker = None

    for mod in get_tasks_from_playbook(playbook_path, line_tracker):
        module_name = mod["module"]
        issues = []
        params = mod.get("params", {})

        # Skip if module_name is not a string (e.g., from custom YAML tags)
        if not isinstance(module_name, str):
            continue

        if "." in module_name:
            short_name = module_name.split(".")[-1]
        else:
            short_name = module_name
        details = migration_map.get(short_name, {})
        # 🔍 Process only for z/OS modules
        if not short_name.startswith("zos_"):
            continue

        # Removed params validation
        for param in details.get("removed_params", []):
            if has_nested_key(params, param):
                issues.append(f"[MUST_FIX] Param '{param}' is removed in {module_name}")

        # Renamed params validation
        for param, new_name in details.get("renamed_params", {}).items():
            if has_nested_key(params, param):
                issues.append(f"[MUST_FIX] Param '{param}' is renamed to '{new_name}' in {module_name}")

        # Type changed params validation
        for param, value in details.get("type_changed_params", {}).items():
            if has_nested_key(params, param):
                old_type, new_type = value.split("_", 1)
                issues.append(
                    f"[MUST_FIX] Param '{param}' type changed from '{old_type}' to '{new_type}' in {module_name}"
                )
        if not ignore_response_params:
            # Removed response parameters validation
            for param in details.get("removed_response_params", []):
                issues.append(f"[WARNING] Response param '{param}' is removed in {module_name}")

            # Renamed response params validation
            for param, new_name in details.get("renamed_response_params", {}).items():
                issues.append(f"[WARNING] Response param '{param}' is renamed to '{new_name}' in {module_name}")

        if issues:
            results.append({
                "playbook": playbook_path,
                "play_name": mod["play_name"],
                "task_name": mod["task_name"],
                "module": module_name,
                "task_line": mod["task_line"],
                "migration_actions": issues
            })
    return results


def main():
    parser = argparse.ArgumentParser(description="Validate Ansible playbooks for migration changes")
    parser.add_argument("--playbook_path", type=str, required=True)
    parser.add_argument("--migration_map", type=json.loads, required=True)
    parser.add_argument("--output_path", type=str, required=True)
    parser.add_argument("--ignore_response_params", action="store_true")

    args = parser.parse_args()

    playbook_path = args.playbook_path
    migration_map = args.migration_map
    output_path = args.output_path
    ignore_response_params = args.ignore_response_params
    all_results = []

    # Check if playbook_path is a file or directory
    if os.path.isfile(playbook_path):
        # Single file - validate it directly
        if playbook_path.endswith((".yml", ".yaml")):
            all_results.extend(validate_tasks(playbook_path, migration_map, ignore_response_params))
    elif os.path.isdir(playbook_path):
        # Directory - walk through all YAML files
        for root, dirs, files in os.walk(playbook_path):
            for file in files:
                if file.endswith((".yml", ".yaml")):
                    path = os.path.join(root, file)
                    all_results.extend(validate_tasks(path, migration_map, ignore_response_params))
    else:
        print(json.dumps({
            "failed": True,
            "msg": f"Invalid path: {playbook_path} is neither a file nor a directory"
        }), file=sys.stderr)
        sys.exit(1)

    try:
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        with open(output_path, 'w') as out:
            json.dump(all_results, out, indent=2)
    except Exception as e:
        print(json.dumps({
            "failed": True,
            "msg": f"Failed to write output to file: {str(e)}"
        }), file=sys.stderr)
        sys.exit(1)

    result = {
        "playbook_path": playbook_path,
        "output_path": output_path,
        "results": all_results
    }
    print(json.dumps(result))
    sys.exit(0)


if __name__ == "__main__":
    main()
