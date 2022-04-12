# Manage z/OS Certificates

This playbook demonstrates how to copy data to and from a z/OS system using modules included in the Red Hat Ansible Certified Content for IBM Z core collection.

This project contains playbooks and roles that demonstrates certificate renewal
using RACF. The playbooks in this project are designed to address an end to end
scenario managing z/OS certificates beginning with monitoring certificates using
Health Checker, creating an authority and certificates, deleting certificates
and even renewing them.

ALthough this is an end to end scenario, you can choose to run the individual
playbooks if you are interested in one particular operation. Review the individual
playbook for more details.

In addition to providing the various operations, this project goes further and
demonstrates how some of the same operations can be run various ways. For example,
you will notice that the two modules `zos_mvs_raw` and `zos_tso_command` are
heavily used to perform the same operation. You can choose to create a certificate
authority using the playbook based on `zos_mvs_raw` which is
[**create_CERTAUTH_cert_raw.yml**](create_CERTAUTH_cert_raw.yml) or you could
choose the playbook leveraging TSO commands to do the same thing
[**create_CERTAUTH_cert_tsocmd.yml**](create_CERTAUTH_cert_tsocmd.yml). Either
playbook will yield the same results and this offers you the opportunity to
explore multiple ways to do the same operation.

These playbook use:

    collection:
        ibm.ibm_zos_core
    modules:
        zos_mvs_raw
        zos_tso_command
        zos_operator
        zos_job_submit

It is a good practice to review the playbook contents before executing them.
It will help you understand the requirements in terms of space, location, names,
authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.3.1 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Run the playbook
This project has several playbooks that you can run, choose a `playbook-name`
and substitute it in the command below to execute it.

```bash
ansible-playbook -i inventories <playbook-name>
```

## Playbook
- [**health_checker_security**](health_checker_security.yml) - Set up security profile for accessing Health Checker functions.

## Playbooks Using module `zos_mvs_raw` (operationally the same as playbooks using `zos_tso_command`)
- [**create_CERTAUTH_cert_raw.yml**](create_CERTAUTH_cert_raw.yml) - Create a CERTAUTH certificate using zos_mvs_raw module.
- [**create_SITE_cert_raw.yml**](create_SITE_cert_raw.yml) - Create a SITE certificate using zos_mvs_raw module.
- [**create_USER_cert_raw.yml**](create_USER_cert_raw.yml) - Create a USER certificate using zos_mvs_raw module.
- [**delete_cert_raw.yml**](delete_cert_raw.yml) - Delete a certificate using zos_mvs_raw module.
- [**delete_keyring_raw.yml**](delete_keyring_raw.yml) - Delete a keyring using zos_mvs_raw module.
- [**list_cert_raw.yml**](list_cert_raw.yml) - Display a certificate's details using zos_mvs_raw module.
- [**search_and_renew_raw.yml**](search_and_renew_raw.yml) - Search and a renew a matching certificate found in the RACF_CERTIFCATE_EXPIRATION health check report using zos_mvs_raw module.

## Playbooks Using module `zos_tso_command` (operationally the same as playbooks using `zos_mvs_raw`)
- [**create_CERTAUTH_cert_tsocmd.yml**](create_CERTAUTH_cert_tsocmd.yml) - Create a CERTAUTH certificate using zos_tso_command module.
- [**create_SITE_cert_tsocmd.yml**](create_SITE_cert_tsocmd.yml) - Create a SITE certificate using zos_tso_command module.
- [**create_USER_cert_tsocmd.yml**](create_USER_cert_tsocmd.yml) - Create a USER certificate using zos_tso_command module.
- [**delete_cert_tsocmd.yml**](delete_cert_tsocmd.yml) - Delete a certificate using zos_tso_command module.
- [**delete_keyring_tsocmd.yml**](delete_keyring_tsocmd.yml) - Delete a keyring using zos_tso_command module.
- [**list_cert_tsocmd.yml**](list_cert_tsocmd.yml) - Display a certificate details using zos_tso_command module.
- [**search_and_renew_tsocmd.yml**](search_and_renew_tsocmd.yml) - Search and a renew a matching certificate found in the RACF_CERTIFCATE_EXPIRATION health check report using zos_tso_command module.

## Role Summary
- [**issue_operator_cmd**](roles/issue_operator_cmd/README.md) - Issue an operator command
- [**issue_racf_cmd**](roles/issue_racf_cmd/README.md) - Isuue RACF command(s)
- [**issue_tso_cmd**](roles/issue_operator_cmd/README.md) - Issue TSO command(s)
- [**print_hc_buffer**](roles/print_hc_buffer/README.md) - Pull data from Health Checker
- [**send-template**](roles/send-template/README.md) - send template to a zOS host

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