# Initialize z/OS DASD Volumes

This sample playbook demonstrates how to initialize a z/OS DASD volume and
create, copy over, and submit JCL to a data set on the newly initialized volume
using modules included in the Red Hat Ansible Certified Content for IBM Z core 
collection.

These playbook use:

    collection:
        ibm.ibm_zos_core
    modules:
        zos_ickdsf_init
        zos_data_set
        zos_copy
        zos_job_submit
        zos_job_output

It is a good practice to review the playbook contents before executing them.
It will help you understand the requirements in terms of space, location, names,
authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.6.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.13 or later](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Run the playbook

```bash
ansible-playbook -i inventories init_dasd_vol_and_run_sample_jcl.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2023

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../../../README.md#support) for more
details.