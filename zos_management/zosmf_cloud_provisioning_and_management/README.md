# z/OSMF Cloud Provisioning and Management

This project provides sample playbooks which are used to demonstrate how to
provision and manage z/OS middlewares/softwares, such as CICS, Db2, IMS, and
WebSphere Liberty, on the target z/OS systems using roles provided by IBM
z/OSMF collection included in the Red Hat Ansible Certified Content for IBM Z.

It is a good practice to review the playbook sample contents before executing
them.
It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources.
Review the playbook notes sections for additional details and configuration.

## Playbook Summary

- [**cpm_provision_software_service.yml**](cpm_provision_software_service.yml) -
Provision a z/OS software service on the target z/OS systems using role
`zmf_cpm_provision_software_service`.
This playbook creates a local record file of instance information, which is
returned in JSON format and is used as the input to other playbooks
[cpm_manage_software_instance.yml](cpm_manage_software_instance.yml) and
[cpm_remove_software_instance.yml](cpm_remove_software_instance.yml).

- [**cpm_manage_software_instance.yml**](cpm_manage_software_instance.yml) -
Perform various actions to manage a provisioned software service instance on
the target z/OS systems using role `zmf_cpm_manage_software_instance`.
The actions that can be performed on the provisioned instance are described in
the local record file that is associated with the provisioned instance.
Specifically, the `name` variable in `actions` array under `registry-info`
identifies the various actions that can be performed on the instance.

- [**cpm_remove_software_instance.yml**](cpm_remove_software_instance.yml) -
Remove a deprovisioned software service instance on the target z/OS systems
using role `zmf_cpm_remove_software_instance`.

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
timeout value in seconds, and `command_timeout` to specify the amount of time
to wait for a command or RPC call before timing out.

For example:

``` {.yaml}
[defaults]
collections_paths = ~/.ansible/collections:/usr/share/ansible/collections

[persistent_connection]
connect_timeout = 300
command_timeout = 300
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
    cpm_host1:
      zmf_host: zosmf_host_name1
      zmf_port: zosmf_port_number1
    cpm_host2:
      zmf_host: zosmf_host_name2
      zmf_port: zosmf_port_number2
```

- **zos_systems**: Group name of the target z/OS systems.

- **cpm_host1**: Nickname for each target z/OS system on which the software
instance is to be performed.
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
you organize your variable values.

The sample includes a **host_vars** file
[**cpm_host1.yml**](host_vars/cpm_host1.yml) that can be easily customized.

``` {.yaml}
instance_record_dir: "/tmp"
api_polling_retry_count: 50
api_polling_interval_seconds: 10
```

- **instance_record_dir**: The value of this property identifies the directory
path that the provisioning role uses to capture various information
(in JSON format) about the provisioned instance.

- **api_polling_retry_count**: The value of this property identifies the total
retry attempts allowed before the task exits with failure, waiting on the
instance action to complete.

- **api_polling_interval_seconds**: The value of this property identifies the
interval time (in seconds) for each polling request.

### Run the Playbook

Access the sample Ansible playbook and ensure that you are within the playbook
directory where the sample files are included.

Use the Ansible command `ansible-playbook` to run the sample playbook.
The command syntax is `ansible-playbook -i <inventory> <playbook>`.

For example:

```bash
ansible-playbook -i inventory.yml cpm_provision_software_service.yml
ansible-playbook -i inventory.yml cpm_manage_software_instance.yml
ansible-playbook -i inventory.yml cpm_remove_software_instance.yml
```

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
