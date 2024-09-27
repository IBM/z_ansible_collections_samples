# Unarchive Pax and Set Custom Facts for z/OS

These sample playbooks demonstrate how to unpackage and install a z/OS program
from a pax file on a remote system and how to access and update custom Ansible
facts with Red Hat Ansible Certified Content for IBM Z and built in Red Hat
Ansible features.


These playbooks use:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_copy
    - zos_unarchive


It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.


## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.10.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.14 or later](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)


## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.


## Run the playbook
This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks then it invokes the three included playbooks
`copy_sample_facts_files_to_remote_system.yml`, `gather_and_print_facts.yml` and `unarchive_pax_and_set_custom_facts.yml`

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the included playbooks with
command:

```bash
ansible-playbook -i inventories copy_sample_facts_files_to_remote_system.yml
ansible-playbook -i inventories gather_and_print_facts.yml
ansible-playbook -i inventories unarchive_pax_and_set_custom_facts.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2024

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../README.md#support) for
more details.