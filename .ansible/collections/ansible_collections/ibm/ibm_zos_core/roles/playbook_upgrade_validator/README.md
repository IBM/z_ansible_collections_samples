# ibm.ibm_zos_core.playbook_upgrade_validator

This role builds a report to migrate playbooks to v2.0.0.

## Role Variables

The following variables are required:

- `playbook_upgrade_validator_playbook_path`: Path to an Ansible playbook or
  a directory containing playbooks to validate.
- `playbook_upgrade_validator_output_path`: Path to the output JSON file where
  results should be saved.
- `playbook_upgrade_validator_ignore_response_params`: Indicates whether
  information about response parameter changes should be included.
