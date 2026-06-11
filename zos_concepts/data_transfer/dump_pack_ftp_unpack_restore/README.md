# Transfer Data Sets Using Ansible

This project provides sample playbooks and roles which can be used to transfer
data sets between z/OS systems using Red Hat Ansible Certified Content for IBM Z.

This playbook uses:

- collection:
  - ibm.ibm_zos_core
- modules:
  - zos_data_set
  - zos_mvs_raw

It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

> [!IMPORTANT]
> The release of `ibm_zos_core` collection version 2.0.0 introduced option
> and return value changes to modules. See the porting
> guide in the release notes
> ([here](https://ibm.github.io/z_ansible_collections_doc/ibm_zos_core/docs/source/release_notes.html#porting-guide))
> to understand the full scope of these changes.
>
> These changes are not compatible with earlier versions of the `ibm_zos_core` collection,
> and since the earlier versions do not reach their end of life until February 2028,
> the sample playbooks compatible with the older versions will remain available.
>
> To support both the latest and prior versions of the collection:
>
> - The `main` branch will reflect playbooks compatible with `ibm_zos_core` version 2.0.0 or later.
>   - Find this playbook on the `main` branch: [here](https://github.com/IBM/z_ansible_collections_samples/tree/main/zos_concepts/data_transfer/dump_pack_ftp_unpack_restore).
> - The `ibm_zos_core-v1.16.0-samples` branch will reflect playbooks compatible with `ibm_zos_core` version 1.16.x or earlier.
>   - Find this playbook on the `ibm_zos_core-v1.16.0-samples` branch: [here](https://github.com/IBM/z_ansible_collections_samples/tree/ibm_zos_core-v1.16.0-samples/zos_concepts/data_transfer/dump_pack_ftp_unpack_restore).
>
> The `ibm_zos_core-v1.16.0-samples` branch will remain available until February 2028 but will not receive updates.

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

- [IBM® z/OS® core collection 2.0.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.16 to 2.19](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

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

© Copyright IBM Corporation 2020, 2026

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../../README.md#support) for more
details.
