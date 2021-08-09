# Data Set Basics

This sample playbook demonstrates basic use cases for manipulating data sets on
z/OS using Red Hat Ansible Certified Content for IBM Z.

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

Although playbooks are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it can't always
be determined if a sample has access to the host’s resources. Review the
playbook for additional details and configuration.

## Playbook Requirement

* IBM z/OS core collection 1.2.0 or later

Note, collections will have requirements and dependencies that are not listed
here. Please review the collections requirements section you have installed
before running this playbook.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

### Update the included [inventory](inventory) with the information about your system(s)

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

### Run desired playbook

This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the `data_set_basics.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventory site.yml
```

You can skip the prerequisite check and run the `data_set_basics.yml` with
command:

```bash
ansible-playbook -i inventory data_set_basics.yml
```

# Copyright

© Copyright IBM Corporation 2020, 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../../README.md#support) for more
details.