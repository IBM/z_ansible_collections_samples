# Manage z/OS Users Using Ansible
This project provides playbooks and roles which can be used to create and
remove users from a z/OS system using modules included in the
Red Hat Ansible Certified Content for IBM Z core collection.

This playbook uses:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_copy
    - zos_data_set
    - zos_tso_command

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbook Summary

- [**add-user.yml**](add-user.yml) - Handles adding a user to z/OS. Playbook includes; group configuration, granting permissions, generating passwords, creating and mounting ZFS filesystem for OMVS, transferring files and templates, creating generic profile, and creating catalog alias. Playbook is designed to be used standalone or in an Ansible Tower workflow template.
- [**remove-user.yml**](remove-user.yml) - Handles removal of a user from z/OS. Playbook includes removal of all configuration performed in **add-user.yml**. Playbook is designed to be used standalone or in an Ansible Tower workflow template.
- [**send-rejection-email.yml**](send-rejection-email.yml) - Handles sending rejection email in the event a user's request for a new UserID is rejected. Playbook is designed to be used in an Ansible Tower workflow template.

## Role Summary

- [**add-zos-user**](roles/add-zos-user/README.md) - Holds tasks related to adding a z/OS user.
- [**remove-zos-user**](roles/remove-zos-user/README.md) - Holds tasks related to removing a z/OS user.

## Playbook Requirements

This playbook requires:
- [IBM® z/OS® core collection 1.2.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Run the playbook
This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the  `add-user.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the `add-user.yml` or `remove-user` with command:

```bash
ansible-playbook -i inventories add-user.yml
ansible-playbook -i inventories add-user.yml
```

## Getting Started: Ansible Tower
Please refer to the [Tower setup documentation](tower-setup.md) for a step-by-step guide for getting user management configured in Ansible Tower.

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
