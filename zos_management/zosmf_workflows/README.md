# Invoking z/OSMF Workflows

This project provides sample playbooks which are used to demonstrate how to
work with z/OSMF workflows on the target z/OS systems using modules and roles
provided by IBM z/OSMF collection included in the Red Hat Ansible Certified
Content for IBM Z.

It is a good practice to review the playbook sample contents before executing
them.
It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources.
Review the playbook notes sections for additional details and configuration.

## Playbook Summary

- [**workflow_basic.yml**](workflow_basic.yml) - Perform various operations
with z/OSMF workflows on the target z/OS systems using module `zmf_workflow`.
This playbook checks whether a workflow instance with the given name exists,
deletes the workflow instance if already exists, creates a new workflow
instance and start it, and then checks the status of the workflow instance.

- [**workflow_complete.yml**](workflow_complete.yml) - Complete a z/OSMF
workflow forcibly or idempotently on the target z/OS systems using role
`zmf_workflow_complete`.

## Ansible Collection Requirement

IBM z/OSMF collection 1.0.0 or later

## Getting Started

### Ansible Config

The Ansible configuration file **ansible.cfg** can override almost all
`ansible-playbook` configurations.
The configuration file includes a sample [**ansible.cfg**](ansible.cfg) that can
supplement `ansible-playbook` with a little modification.

You can modify `collections_paths` to refer to your own installation path for
Ansible collections.
You can also specify `connect_timeout` to specify the persistent connection
timeout value in seconds.

For example:

``` {.yaml}
[defaults]
collections_paths = ~/.ansible/collections:/usr/share/ansible/collections

[persistent_connection]
connect_timeout = 300
```

For more information about available configurations for `ansible.cfg`,
read the Ansible documentation on
[Ansible configuration settings](https://docs.ansible.com/ansible/latest/reference_appendices/config.html#ansible-configuration-settings-locations).

### Inventory

Ansible works with multiple managed nodes (hosts) at the same time,
using a list or group of lists known as an
[inventory](https://docs.ansible.com/ansible/latest/user_guide/intro_inventory.html).
Once the inventory is defined, you can use
[patterns](https://docs.ansible.com/ansible/latest/user_guide/intro_patterns.html#intro-patterns)
to select the hosts or groups that you want Ansible to run on.

The sample includes [**inventory.yml**](inventory.yml) that can be used to manage
your target z/OS systems with a little modification.
This inventory file should be included when running the sample playbooks.

``` {.yaml}
zos_systems:
  hosts:
    workflow_host1:
    workflow_host2:
```

- **zos_systems**: Group name of the target z/OS systems.

- **workflow_host1**: Nickname of each target z/OS system on which the
workflow instance is to be performed.
You can modify it to refer to your own z/OS system.
You need to ensure the z/OS system `workflow_host1` or your own z/OS system
is configured in **z/OSMF Systems** plugin.

### Variables

Although you can store variables in the **inventory** file, storing them in
separate configurations such as **host_vars** or **group_vars** files help
you organize your variable values. **host_vars** file name must match the host
name used in the **inventory** file and sample playbooks.

The sample includes a **group_vars** file
[**zos_systems.yml**](group_vars/zos_systems.yml) that can be easily
customized.

``` {.yaml}
zmf_host: zosmf_host_name
zmf_port: zosmf_port_number
# zmf_user: zosmf_user_name
# zmf_password: zosmf_password
# zmf_crt: zosmf_client_certificate
# zmf_key: zosmf_client_certificate_key
```

- **zmf_host**: The value of this property identifies the hostname of the z/OS
system on which z/OSMF server is running on.
For example: `zmf_host: "pev076.pok.ibm.com"`.

- **zmf_port**: The value of this property identifies the port number of
z/OSMF server.

- **zmf_user**: The value of this property identifies the username to be used
for authenticating with z/OSMF server.

- **zmf_password**: The value of this property identifies the password to be
used for authenticating with z/OSMF server.

- **zmf_crt**: The value of this property identifies the location of the
PEM-formatted certificate chain file to be used for HTTPS client
authentication with z/OSMF server.

- **zmf_key**: The value of this property identifies the location of the
PEM-formatted file with private key to be used for HTTPS client
authentication with z/OSMF server.

**`Notes:`**

- This is an easy example to use username and password for authenticating with
z/OSMF server.
**zmf_user** and **zmf_password** will be prompted to input when running the
sample playbooks.
Actually, client-certificate authorization is recommended.
You can use **zmf_crt** and **zmf_key** to specify the client-certificate
authorization.
If both methods are specified, the system attempts to use client-certificate
authentication.

### Run the Playbook

Access the sample Ansible playbook and ensure that you are within the playbook
directory where the sample files are included.

Use the Ansible command `ansible-playbook` to run the sample playbook.
The command syntax is `ansible-playbook -i <inventory> <playbook>`.

For example:

```bash
ansible-playbook -i inventory.yml workflow_basic.yml
ansible-playbook -i inventory.yml workflow_complete.yml
```

**`Notes:`**

To run the sample playbooks, below preparation works are required:

- It is recommended that you use the naming rule
`workflow_name: "ansible_${ workflow_name }_{{ inventory_hostname }}"` when
creating a workflow instance.

- Submitting a z/OSMF workflow found on Ansible control node is currently not
supported.
The workflow definition file
[workflow_sample_automation_steps.xml](files/workflow_sample_automation_steps.xml)
is used to create the workflow instance.
You need to manually upload it to the target z/OS system.
For example, you can upload it to the directory `/var/zosmf/workflow_def/`,
then modify the value of variable **workflow_file** in the sample playbook to
refer to the location of the workflow definition file.

- Only automated steps are supported when starting a z/OSMF workflow.

- In sample playbook [**workflow_basic.yml**](workflow_basic.yml), module
`zmf_authenticate` is supported by z/OSMF APAR PH12143 (PTF UI66511 for V2R3,
PTF UI66512 for V2R4).
You are also allowed to authenticate with z/OSMF server with module
`zmf_workflow` directly.

### Debugging

Optionally, you can configure the console logging verbosity during playbook
execution.
This is helpful in situations where communication is failing and you want to
obtain more details.
To adjust the logging verbosity, append more letter `v`'s.
For example, `-v`, `-vv`, `-vvv`, or `-vvvv`.
Each letter `v` increases logging verbosity similar to traditional logging
levels INFO, WARN, ERROR, DEBUG.

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](../../README.md#support) for more
details.
