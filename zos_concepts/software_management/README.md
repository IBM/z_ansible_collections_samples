# Apply ptf using Using Ansible

This project provides sample playbooks and roles which can be used to perform SMPE operations.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**setup_internet_retrieval.yml**](setup_internet_retrieval.yml) - Set up IBM Internet Service Retrieval by adding digital certificates to RACF database.
- [**order.yml**](order.yml) - Handles order a PTF from IBM and receive it into the Global zone using IBM Internet Service Retrieval.
- [**install.yml**](install.yml) - Handles APPLY process for a PTF.
- [**uninstall.yml**](uninstall.yml) - Handles RESTORE process for a PTF.
- [**accept.yml**](accept.yml) - Handles ACCEPT process for a PTF.
- [**reject.yml**](reject.yml) - Handles REJECT process for a PTF.
- [**query_csi.yml**](query_csi.yml) - Query CSI data.

## Role Summary

- [**add_certificate**](roles/add_certificate/README.md) - Holds tasks related to adding certificates to RACF.
- [**order_ptf**](roles/order_ptf/README.md) - Holds tasks related to order a PTF from IBM.
- [**apply_ptf**](roles/apply_ptf/README.md) - Holds tasks related to APPLY a PTF.
- [**restore_ptf**](roles/restore_ptf/README.md) - Holds tasks related to RESTORE a PTF.
- [**reject_ptf**](roles/reject_ptf/README.md) - Holds tasks related to REJECT a PTF.
- [**accept_ptf**](roles/accept_ptf/README.md) - Holds tasks related to ACCEPT a PTF.
- [**query_csi**](roles/query_csi/README.md) - Holds tasks related to QUERY data from CSI.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.3.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.
- Update the playbook specific variables found in each playbook and review the
  role README files.

##Run the playbook



This project has included a `site.yml` playbook that serves as the primary playbook
that provides additional prerequisite checks.

If you want to run the primary playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the primary playbook, use command:

```bash
ansible-playbook -i inventories site.yml
```

You can skip the prerequisite check and run the appropriate playbook with
command:

```bash
ansible-playbook -i inventories <playbook-name>
```

# Copyright
© Copyright IBM Corporation 2021

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support
Please refer to the [support section](../../README.md#support) for more
details.
