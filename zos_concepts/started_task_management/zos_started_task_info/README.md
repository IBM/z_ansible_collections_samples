# Display status information for z/OS started tasks

This playbook demonstrates how to retrieve detailed status information for long-running started tasks using Red Hat Ansible Certified Content for IBM Z.

This playbook uses:

- collection:
  - ibm.ibm_zos_core
- modules:
  - zos_started_task
  - zos_started_task_info
  - zos_data_set
  - zos_operator
  - zos_job_query
  - zos_copy

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements

This playbook requires:

- [IBM® z/OS® core collection 2.3.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.16 or later](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration

- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Playbook Variables

The following variables can be configured in [zos_started_task_info.yml](zos_started_task_info.yml) or overridden via variables:

| Variable            | Default Value                           | Description                                                                                           |
| :------------------ | :-------------------------------------- | :---------------------------------------------------------------------------------------------------- |
| `jcl_file`          | `"{{ playbook_dir }}/files/sample.jcl"` | Path to the local JCL source file containing the command executed by the test started task.           |
| `usr_proclib`       | `"USER.PRIVATE.PROCLIB"`                | Target z/OS Partitioned Data Set (PDS) procedure library accessible by the system for starting tasks. |
| `member_name`       | `"SLEEPJCL"`                            | Member name (up to 8 characters) in `usr_proclib` where the JCL is staged.                            |
| `started_task_name` | `"SLEEP"`                               | Name of the started task (up to 8 characters) used to identify and manage the started task.           |

## Execution Tag Scenarios

The playbook includes tags that allow you to execute specific scenarios:

- **`user_started_task`**: Demonstrates the full started task lifecycle:
  1. Copies local sample JCL to the specified PROCLIB member.
  2. Starts the task (`SLEEP`).
  3. Queries status information with `zos_started_task_info` (`verbose: true`).
  4. Cancels the started task.
  5. Demonstrates error handling when querying a cancelled/inactive task.
  6. Cleans up the test PROCLIB member.

- **`set_slip_trap`**: Demonstrates dynamic address space status changes. _(Note: Must run in combination with `user_started_task` tag)_.
  1. Sets a Program Event Recording (PER) SLIP trap on the `SLEEP` task address space using `zos_operator`.
  2. Displays active SLIP trap definitions.
  3. Queries the started task again using `zos_started_task_info`.
  4. Compares `per_trap` (changes from `false` to `true`) and `address_space_flag` (changes to `PR`) before and after the SLIP trap.
  5. Cleans up and deletes the SLIP trap.

- **`wildcards`**: Demonstrates filtering active started tasks:
  1. Queries tasks by wildcard pattern and identifier (`job_name: "I*"`, `identifier_name: "ICSF"`).
  2. Queries tasks by name pattern (`job_name: "J*"`).
  3. Queries all active address spaces on the system (`job_name: "*"`).

- **`task_id`**: Demonstrates querying started tasks using a system job/task ID:
  1. Queries a running task (such as `ICSF`) using `zos_job_query` to retrieve its `job_id`.
  2. Retrieves detailed status information using `task_id` in `zos_started_task_info`.

## Run the playbook

This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks then it invokes the `zos_started_task_info.yml`
playbook.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run `zos_started_task_info.yml` with command:

```bash
ansible-playbook -i inventories zos_started_task_info.yml
```

### Running Specific Scenarios with Tags

You can use Ansible tags to execute specific scenarios:

```bash
# Run only the user started task lifecycle
ansible-playbook -i inventories zos_started_task_info.yml --tags user_started_task

# Run the user started task lifecycle along with the SLIP trap demonstration
ansible-playbook -i inventories zos_started_task_info.yml --tags "user_started_task,set_slip_trap"

# Run wildcard query examples
ansible-playbook -i inventories zos_started_task_info.yml --tags wildcards

# Run task ID query example
ansible-playbook -i inventories zos_started_task_info.yml --tags task_id
```

# Changelog

All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright

© Copyright IBM Corporation 2026

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../../README.md#support) for more
details.
