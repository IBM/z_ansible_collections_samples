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

Note, only the playbook requirements are listed in this document, please review
the collections documentation for additional requirements.

## Getting Started
If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

### Update the variables in [host_vars/source.yml](host_vars/source.yml) and
[host_vars/destination.yml](host_vars/destination.yml) based on desired operations

### Update the included [inventory](inventory) with the information about your system's.
Description of the properties used in this configuration:
* Property `ansible_host` is the z/OS managed node (target), e.g, `ansible_host: "zvm1.vmec.svl.ibm.com"`
* Property `ansible_user` is the z/OS managed user to connect and run as over SSH,  e.g, `ansible_user: "zosadm"`
* Property `pyz` is the python installation home path on the z/OS managed node (target), e.g, `pyz: "/usr/lpp/IBM/cyp/v3r8/pyz"`
* Property `ansible_python_interpreter` is the z/OS managed node (target) Python binary installation path,
  e.g, `ansible_python_interpreter: "{{pyz}}/bin/python3.8"`
* Property `zoau` is the ZOAU installation home on the z/OS managed node (target), e.g, `zoau: "/usr/lpp/IBM/zoautil"`

```yaml
# The system which initially holds the data sets
source_system:
  hosts:
    source:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      pyz: path_to_python_installation_on_zos_target
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
      zoau: path_to_zoau_installation_on_zos_target

# The system which should receive a copy of the data sets on source_system
destination_system:
  hosts:
    destination:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      pyz: path_to_python_installation_on_zos_target
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
      zoau: path_to_zoau_installation_on_zos_target
```

### Run the playbook
This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the `transfer-data-sets.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventory.yml site.yml
```

You can skip the prerequisite check and run the `transfer-data-sets.yml`,
`dump-pack-ftp.yml` or `unpack-restore.yml` with
command:

```bash
ansible-playbook -i inventory.yml transfer-data-sets.yml
ansible-playbook -i inventory.yml dump-pack-ftp.yml
ansible-playbook -i inventory.yml unpack-restore.yml
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
