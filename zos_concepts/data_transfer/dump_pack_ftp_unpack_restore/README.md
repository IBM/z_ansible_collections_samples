# Transfer Data Sets Using Ansible

This project provides sample playbooks and roles which can be used to transfer
data sets between z/OS systems using Red Hat Ansible Certified Content for IBM Z.

following core modules are used to accomplish these set of tasks:

This playbook uses:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_data_set
    - zos_mvs_raw

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbook Summary
- [**transfer-data-sets.yml**](transfer-data-sets.yml) - Handles full transfer process: dump, pack, FTP, unpack and restore.
- [**dump-pack-ftp.yml**](dump-pack-ftp.yml) - Handles transfer of data sets: dump, pack and FTP.
- [**unpack-restore**](unpack-restore.yml) - Handles unpack and restore of data sets from a terse of a data set dump.

## Role Summary
- [**dump-pack-ftp-data-sets**](roles/dump-pack-ftp-data-sets/README.md) - Holds tasks related to transfer of data sets: dump, pack and FTP.
- [**unpack-restore-data-sets**](roles/unpack-restore-data-sets/README.md) - Holds tasks related to unpack and restore of data sets from a terse of a data set dump.
- [**get-estimated-size-of-data-sets**](roles/get-estimated-size-of-data-sets/README.md) - Estimates the total amount of storage space used by one or more data sets.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.2.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

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
that provides additional prerequisite checks then it invokes the `transfer-data-sets.yml`
playbook.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the `transfer-data-sets.yml`,
`dump-pack-ftp.yml` or `unpack-restore.yml` with
command:

```bash
ansible-playbook -i inventories transfer-data-sets.yml
ansible-playbook -i inventories dump-pack-ftp.yml
ansible-playbook -i inventories unpack-restore.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2020

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support
Please refer to the [support section](../../../README.md#support) for more
details.
