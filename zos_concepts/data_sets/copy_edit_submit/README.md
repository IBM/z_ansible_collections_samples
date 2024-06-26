# Copy Directory to PDS, Edit member and Submit Job
This playbook demonstrates how to copy a directory to a PDS, edit a
member within the PDS and submit the PDS member containing JCL.

This playbook uses:
  - collection:
    - ibm.ibm_zos_core
  - modules:
    - zos_copy
    - zos_lineinfile
    - zos_job_submit

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

> [!IMPORTANT]
> The release of `ibm_zos_core` collection version 1.10.0 introduced case-sensitive
> changes to modules, specifically that module choices must be lower case. See the porting
> guide in the release notes
> ([here](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html#porting-guide))
> to understand the full scope of these changes.
>
> These changes are not compatible with earlier versions of the `ibm_zos_core` collection, 
> and since the earlier versions do not reach their end of life until April 2025, 
> the sample playbook compatible with the older versions will remain available.
>
> To support both the latest and prior versions of the collection: 
> * The `main` branch will reflect playbooks compatible with `ibm_zos_core` version 1.10.0 or later.
>   * Find this playbook on the `main` branch: [here](https://github.com/IBM/z_ansible_collections_samples/tree/main/zos_concepts/data_sets/copy_edit_submit).
> * The `ibm_zos_core-v1.9.x-samples` branch will reflect playbooks compatible with `ibm_zos_core` version 1.9.x or earlier.
>   * Find this playbook on the `ibm_zos_core-v1.9.x-samples` branch: [here](https://github.com/IBM/z_ansible_collections_samples/tree/ibm_zos_core-v1.9.x-samples/zos_concepts/data_sets/copy_edit_submit).
> 
> The `ibm_zos_core-v1.9.x-samples` branch will remain available until April 2025 but will not receive updates.
>
>

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

This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks then it invokes the `copy_edit_submit.yml`
playbook.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the `copy_edit_submit.yml` with
command:

```bash
ansible-playbook -i inventories copy_edit_submit.yml
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
