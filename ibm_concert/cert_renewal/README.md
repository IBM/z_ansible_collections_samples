# Manage z/OS Certificates using IBM Concert/ServiceNow/Ansible Automation Platform

These sample playbooks when used with IBM Concert, SericeNow, and Ansible Automation Platform
can simplify the tracking and resolving RACF certificate issues.

[**send_cert_data.yml**](send_cert_data.yml) this playbook will run the z/OS Health Checker RACF Certificate Expiriation report and pull the data
into a CSV file and send it to a IBM Concert instance
[**renew_cert.yml**](renew_cert.yml) this playbook will renew a z/OS certificate using the certificate data sent in from a ServiceNow REST message to Ansible Automation Platform

These playbook use:

    collections:
      - name: ibm.ibm_zos_core
        version: 1.10.0
      - name: ibm.ibm_zos_ims
        version: 1.3.0
      - name: ansible.posix
        version: 1.5.4
      - name: ansible.utils
        version: 4.1.0
      - name: community.general
        version: 9.4.0
      - name: servicenow.itsm
        version: 2.7.0


It is a good practice to review the playbook contents before executing them.
It will help you understand the requirements in terms of space, location, names,
authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.10.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Configuration
- Configure the included [inventory.yml](inventories/inventory.yml) with the
  information from the managed z/OS host.
  - Review [inventory documentation](../docs/share/zos_core/configure_inventory.md)
- Configure the included **host_vars** [zos_host.yml](inventories/host_vars/zos_host.yml)
  with the information from your z/OS system.
  - Review [host_vars documentation](../../docs/share/zos_core/configure_host_vars.md)
    and any additional noted variables in the configuration.

## Set up job templates on Ansible Automation Platform
This project has several playbooks that you can run, choose a `playbook-name`
and substitute it in the command below to execute it.

```bash
ansible-playbook -i inventories <playbook-name>
```

## Playbook
- [**health_checker_security**](health_checker_security.yml) - Set up security profile for accessing Health Checker functions.

## Role Summary
- [**issue_operator_cmd**](roles/issue_operator_cmd/README.md) - Issue an operator command
- [**issue_racf_cmd**](roles/issue_racf_cmd/README.md) - Isuue RACF command(s)
- [**issue_tso_cmd**](roles/issue_tso_cmd/README.md) - Issue TSO command(s)
- [**print_hc_buffer**](roles/print_hc_buffer/README.md) - Pull data from Health Checker
- [**send-template**](roles/send-template/README.md) - send template to a zOS host

## Set up ServiceNow for Ansible Automation Platform Integration
- Create REST message
- Create business rule to send REST message when Incident tickets State change
- Customize script to send correct inputs to AAP job templates
- Change State to 'In progress' to kick off the renewal process

## Using IBM Concert to track and resolve expired Certificates
- Set up AAP schedule to send cert data to IBM Concert on a regular basis
- Create ServiceNow ticket to renew a cert


# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2024

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../../README.md#support) for more
details.