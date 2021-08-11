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

Note, only the playbook requirements are listed in this document, please review
the collections documentation for additional requirements.

## Getting Started
If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

### Update the included [inventory.yml](inventory.yml) with the information about your system's.
Description of the properties used in this configuration:
* Property `ansible_host` is the z/OS managed node (target), e.g, `ansible_host: "zvm1.vmec.svl.ibm.com"`
* Property `ansible_user` is the z/OS managed user to connect and run as over SSH,  e.g, `ansible_user: "zosadm"`
* Property `pyz` is the python installation home path on the z/OS managed node (target), e.g, `pyz: "/usr/lpp/IBM/cyp/v3r8/pyz"`
* Property `ansible_python_interpreter` is the z/OS managed node (target) Python binary installation path,
  e.g, `ansible_python_interpreter: "{{pyz}}/bin/python3.8"`
* Property `zoau` is the ZOAU installation home on the z/OS managed node (target), e.g, `zoau: "/usr/lpp/IBM/zoautil"`

```yaml
source_system:
  hosts:
    zos_host:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      pyz: path_to_python_installation_on_zos_target
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
      zoau: path_to_zoau_installation_on_zos_target
```

### Update the playbook specific variables in [host_vars/zos_host.yml](host_vars/zos_host.yml) based on desired behavior

### Run desired playbook
This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the  `add-user.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventory.yml site.yml
```

You can skip the prerequisite check and run the `add-user.yml` with command:

```bash
ansible-playbook -i inventory.yml add-user.yml
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
