# Query attributes from data sets, Generation Data Groups, aggregates and Unix System Services files
This playbook demonstrates how to get information about the attributes
of different types of resources in a z/OS system using Red Hat Ansible
Certified Content for IBM Z.

This playbook uses:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_copy
    - zos_data_set
    - zos_started_task

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.16.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.16 or later](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Run the playbook
This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks then it invokes the `zos_started_task.yml`
playbook.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run `zos_started_task.yml` with command:

```bash
ansible-playbook -i inventories zos_started_task.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2025

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../README.md#support) for more
details.
