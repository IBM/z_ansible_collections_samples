# Manage z/OS Certificates

This playbook demonstrates how to copy data to and from a z/OS system using modules included in the Red Hat Ansible Certified Content for IBM Z core collection.

This project contains playbooks and roles that demonstrates certificate renewal
using RACF. The playbooks in this project are designed to address an end to end
scenario managing z/OS certificates beginning with monitoring certificates using
Health Checker, creating an authority and certificates, deleting certificates
and even renewing them.

Although this is an end to end scenario, you can choose to run the individual
playbooks if you are interested in one particular operation. Review the individual
playbook for more details.

These playbook use:

    collection:
        ibm.ibm_zos_core
    modules:
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
  - Review [inventory documentation](../../docs/share/zos_core/configure_inventory.md)
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
1. [**create_cert.yml**](create_cert.yml) - Create a SITE, CERTAUTH, or USER certificate.
2. [**delete_cert.yml**](delete_cert.yml) - Delete a certificate.
3. [**create_keyring.yml**](create_keyring.yml) - Create a keyring.
4. [**delete_keyring.yml**](delete_keyring.yml) - Delete a keyring.
5. [**list_cert.yml**](list_cert.yml) - Display a certificate details.
6. [**health_checker_security**](health_checker_security.yml) - Set up security profile for accessing Health Checker functions. This must be run before search_and_renew.yml will work.
7. [**search_and_renew.yml**](search_and_renew.yml) - Search and a renew a matching certificate found in the RACF_CERTIFCATE_EXPIRATION health check report.

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2020, 2021

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../README.md#support) for more
details.