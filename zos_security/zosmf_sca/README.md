# z/OSMF Security Configuration Assistant

This project provides sample playbooks which demonstrate how to validate
security requirements on the target z/OS systems. These playbooks leverage module
`zmf_sca` provided by IBM z/OSMF collection included in the Red Hat Ansible
Certified Content for IBM Z.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources.
Review the playbook notes sections for additional details and configuration.

## Playbook Summary

- [**sca_security_validation.yml**](sca_security_validation.yml) -
Performing security validation using module `zmf_sca` provided with IBM z/OSMF collection.
The playbook will succeed if all requirements are satisified, otherwise it will fail and
return those unsatisified requirements.

- [**sca_security_audit.yml**](sca_security_audit.yml) -
Performing security audit using module `zmf_sca` provided with IBM z/OSMF collection.
The playbook will succeed if no access to any items, otherwise it will fail and
return those unqualified items.


## Ansible Collection Requirement

IBM z/OSMF collection 1.1.0 or later

## Getting Started

### Ansible Config

The Ansible configuration file **ansible.cfg** can override almost all
`ansible-playbook` configurations.
The configuration file includes a sample [**ansible.cfg**](ansible.cfg) that can
supplement `ansible-playbook` with a little modification.

You can modify `collections_paths` to refer to your own installation path for
Ansible collections.

For example:

``` {.yaml}
[defaults]
collections_paths = ~/.ansible/collections:/usr/share/ansible/collections
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
    sca_host1:
      zmf_host: zosmf_host_name1
      zmf_port: zosmf_port_number1
    sca_host2:
      zmf_host: zosmf_host_name2
      zmf_port: zosmf_port_number2
```

- **zos_systems**: Group name of the target z/OS systems.

- **sca_host1**: Nickname for each target z/OS system on which the security
validation is to be performed.
You can modify it to refer to your own z/OS system.
When the nickname is modified, make sure the host specific variables file is
defined as described in [Variables](#Variables).

- **zmf_host**: The value of this property identifies the hostname of the z/OS
system on which z/OSMF server is running on.
For example: `zmf_host: "pev076.pok.ibm.com"`.

- **zmf_port**: The value of this property identifies the port number of
z/OSMF server.

### Variables

Although you can store variables in the **inventory** file, storing them in
separate configurations such as **host_vars** or **group_vars** files help
you organize your variable values. **host_vars** file name must match the host
name used in the **inventory** file and sample playbooks.

The sample includes a **host_vars** file
[**sca_host1.yml**](host_vars/sca_host1.yml) that can be easily customized.

``` {.yaml}
# target_userid: target_userid
# location: local or remote
# path: /path/to/security/requirement/file
# zmf_user: zosmf_user_name
# zmf_password: zosmf_password
```

- **target_userid**: The value of this property identifies the
target user/group id to be validated.

- **location**: The value of this property identifies the location of
the security requirement file, local or remote.

- **path**: The value of this property identifies the path of
the security requirement file.

- **zmf_user**: The value of this property identifies the username to be used
for authenticating with z/OSMF server.

- **zmf_password**: The value of this property identifies the password to be
used for authenticating with z/OSMF server.

**`Notes:`**

- **zmf_user** and **zmf_password** will be prompted to input when running the
sample playbooks.

### Run the Playbook

Access the sample Ansible playbook and ensure that you are within the playbook
directory where the sample files are included.

Use the Ansible command `ansible-playbook` to run the sample playbook.
The command syntax is `ansible-playbook -i <inventory> <playbook>`.

For example:

```bash
ansible-playbook -i inventory.yml sca_security_validation.yml
ansible-playbook -i inventory.yml sca_security_audit.yml
```

**`Notes:`**

To run the sample playbooks, below preparation works are required:

- Please refer to the sample security requirements file
[sca_sample_security_requirements_file.json](files/sca_sample_security_requirements_file.json)
for security requirements file format.

- In sample playbooks, module
`zmf_authenticate` is supported by z/OSMF APAR PH12143 (PTF UI66511 for V2R3,
PTF UI66512 for V2R4).
You are also allowed to authenticate with z/OSMF server with module
`zmf_sca` directly.

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
