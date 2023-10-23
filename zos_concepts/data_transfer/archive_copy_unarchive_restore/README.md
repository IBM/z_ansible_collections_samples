# Transfer Data Sets Using Ansible

This project provides sample playbooks and roles which can be used to transfer
data sets between z/OS systems using Red Hat Ansible Certified Content for IBM Z.

The following core modules are used to accomplish this set of tasks:

This playbook uses:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_archive
    - zos_unarchive
    - zos_data_set

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbooks Summary
- [**archive_fetch_data_sets.yml**](archive_fetch_data_sets.yml) - Dumps and archive of data sets.
- [**unarchive_data_sets.yml**](unarchive_data_sets.yml) - Transfers, unarchives, and restores the target data set archive.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.7.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 to 2.15](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** files [source.yml](inventories/host_vars/source.yml)
  and [destination.yml](inventories/host_vars/destination.yml) with the
  information from your z/OS system.
  - Review [host_vars documentation](../../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Run the playbook
This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks then it invokes `archive_fetch_data_sets.yml`
playbook and then `unarchive_data_sets.yml`.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the `archive_fetch_data_sets.yml` or
`unarchive_data_sets.yml` with
command:

```bash
ansible-playbook -i inventories archive_fetch_data_sets.yml
ansible-playbook -i inventories unarchive_data_sets.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2023

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support
Please refer to the [support section](../../../README.md#support) for more
details.