# Synchronize APF authorized libraries on z/OS from a configuration file cloned from GitHub
This playbook demonstrates how to clone a Git repository to the user provided
directory on the remote z/OS system using Red Hat Ansible Certified Content for
IBM Z. It then loads the master configuration file containing a set of
libraries (data sets) that are required to be present on the target z/OS
system's APF authorized list.

It then generates a list of libraries to be added to APF authorized list by
comparing the the current APF list and the master list in GitHub. Lastly, it
makes the APF statement entries in the user specified data set or data set
member.

The following core modules are used to accomplish these set of tasks:

This playbook uses:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_find
    - zos_lineinfile
    - zos_mvs_raw
    - zos_fetch

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.2.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)
- [Git for z/OS](https://www.rocketsoftware.com/product-categories/mainframe/git-for-zos)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

### Configuration file format

    <library> [volume]
    ...

```
APFTEST.PGRM001.LIB001 T60313
APFTEST.PGRM001.LIB002 T60314
APFTEST.PGRM001.LIB003 T60315
```

## Run the playbook
This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the `prog_auth.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the `prog_auth.yml` with
command:

```bash
ansible-playbook -i inventories prog_auth.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2020, 2021

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../../README.md#support) for more
details.