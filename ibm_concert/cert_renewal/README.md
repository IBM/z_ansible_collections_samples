# Manage z/OS Certificates using IBM Concert, ServiceNow, and Ansible Automation Platform

These sample playbooks when used with IBM Concert, ServiceNow, and Ansible Automation Platform
can simplify the monitoring and resolution of RACF certificate issues.

It is a good practice to review the playbook contents before executing them.
It will help you understand the requirements in terms of space, location, names,
authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
These playbooks use:

    collections:
      - name: ibm.ibm_zos_core
        version: 1.10.0
      - name: ansible.posix
        version: 1.5.4
      - name: ansible.utils
        version: 4.1.0
      - name: community.general
        version: 9.4.0
      - name: servicenow.itsm
        version: 2.7.0

## Playbooks
[**send_cert_data.yml**](send_cert_data.yml) this playbook will run the z/OS Health Checker RACF Certificate Expiration report and pull the data into a CSV file and send it to an IBM Concert instance for visualization and management.

[**renew_cert.yml**](renew_cert.yml) this playbook will renew a z/OS certificate using the certificate data sent in from a ServiceNow REST message using a job template on Ansible Automation Platform.

## Role Summary
- [**get_cert_detail**](roles/get_cert_detail/README.md) - Retrieve certificate details from RACF and build a CSV record
- [**issue_operator_cmd**](roles/issue_tso_cmd/README.md) - Issue z/OS system command(s)
- [**issue_tso_cmd**](roles/issue_tso_cmd/README.md) - Issue TSO command(s)
- [**print_hc_buffer**](roles/print_hc_buffer/README.md) - Pull data from Health Checker
- [**send-template**](roles/send-template/README.md) - send template to a zOS host

## Set up job templates on Ansible Automation Platform
These playbooks are designed to be used with Ansible Automation Platform (AAP) job templates. The information in the `host_vars` can be used to set up Inventory and Hosts on AAP.

Review the required inputs to each playbooks to set up Surveys on AAP so that external callers can call the AAP REST API correctly.

- Set up an AAP [schedule](https://docs.ansible.com/automation-controller/latest/html/userguide/scheduling.html) to send cert data to IBM Concert on a regular basis
- Set up an AAP [job template](https://docs.ansible.com/automation-controller/latest/html/userguide/job_templates.html#create-a-job-template) to renew a certificate on z/OS when requested by a REST caller

## Set up ServiceNow for Ansible Automation Platform Integration
- Create an Outbound REST message and a POST method to interact with AAP
- Create a business rule to send the POST REST message when an Incident ticket State changes
- Customize a script to send the correct inputs to the AAP job template to renew the certificate on z/OS

## Using IBM Concert to track and resolve expired Certificates
- Create a ServiceNow incident ticket to renew a certificate

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