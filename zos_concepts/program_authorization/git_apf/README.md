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

### Configuration file format

    <library> [volume]
    ...

```
APFTEST.PGRM001.LIB001 T60313
APFTEST.PGRM001.LIB002 T60314
APFTEST.PGRM001.LIB003 T60315
```

### Run the playbook
This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the `prog_auth.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventory.yml site.yml
```

You can skip the prerequisite check and run the `prog_auth.yml` with
command:

```bash
ansible-playbook -i inventory.yml prog_auth.yml
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