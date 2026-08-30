# gather_diagnostics role

This role gathers diagnostic information from the Ansible control node and
target z/OS managed nodes.

It is designed to be run against z/OS hosts, and it will:

1. Gather data from the Ansible control node (localhost).
2. Gather data from each z/OS managed node in the play.

## Role Variables

The following variables, typically defined in `defaults/main.yml`, control
the execution of the role:

* **`gather_diagnostics_gather_control`**
  * **Description**: Controls whether to run the diagnostic tasks on the
    Ansible control node (localhost).
  * **Type**: `bool`
  * **Default**: `true`

* **`gather_diagnostics_gather_managed_node`**
  * **Description**: Controls whether to run the diagnostic tasks on the
    z/OS managed nodes.
  * **Type**: `bool`
  * **Default**: `true`

* **`gather_diagnostics_save_control_log`**
  * **Description**: Controls whether the role creates a YAML report file
    (`gather_diagnostics_report_control.yml`) on the control node.
  * If `true` (default), the report file is created, and sensitive data is
    hidden from the console (`no_log: true`).
  * If `false`, no report file is created, and all gathered data is printed
    to the console.
  * **Type**: `bool`
  * **Default**: `true`

* **`gather_diagnostics_save_managed_node_log`**
  * **Description**: Controls whether the role creates a YAML report file
    for each managed node
    (`gather_diagnostics_report_managed_node_<hostname>.yml`) on the control
    node.
  * If `true` (default), the report file is created, and sensitive data is
    hidden from the console (`no_log: true`).
  * If `false`, no report file is created, and all gathered data is printed
    to the console.
  * **Type**: `bool`
  * **Default**: `true`

* **`gather_diagnostics_gather_syslog`**
  * **Description**: Controls whether the role attempts to run authorized
    MVS operator commands (e.g., `D A,L`, `D OMVS,LIMITS`, `D OMVS,OPTIONS`)
    to gather system activity.
  * **Type**: `bool`
  * **Default**: `true`

* **`gather_diagnostics_output_dir`**
  * **Description**: Controls where the role creates YAML reports from
    managed node and control node. In AAP, defaults to artifacts directory;
    otherwise uses control node's home directory.
  * **Type**: `str`
  * **Default**: `{{ playbook_dir }}/artifacts` (AAP) or
    `{{ lookup('env', 'HOME') }}` (standalone)

* **`gather_diagnostics_gather_ssh_config`**
  * **Description**: Controls whether to gather contents of ~/.ssh/rc,
    /etc/ssh/sshrc, /etc/ssh/ssh_config, and /etc/ssh/sshd_config for z/OS
    managed node.
  * **Type**: `bool`
  * **Default**: `false`
