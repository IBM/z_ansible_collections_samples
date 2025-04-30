# Collect z/OS Software instances Topology, Usage, and missing fixes 

These sample playbooks when used with IBM z/OS Management Facility (zOSMF), IBM Z Software Asset Management (IZSAM), IBM Concert, and Ansible Automation Platform (AAP) can simplify the monitoring and resolution of z/OS Software maintenance, planning, and deployment.

It is a good practice to review the playbook contents before executing them.
It will help you understand the requirements in terms of space, location, names,
authority, and the artifacts that will be created and cleaned up.

## Playbook Requirements
These playbooks use:

    collections:
      - name: ibm.ibm_zos_core
        version: 1.10.0
      - name: ibm.ibm_zosmf
        version: 1.5.0
      - name: ansible.posix
        version: 1.5.4
      - name: ansible.utils
        version: 4.1.0
      - name: community.general
        version: 9.4.0

## Playbooks
[**send_data_to_concert.yml**](send_data_to_concert.yml) this playbook will query z/OSMF reports, IZSAM data and send them to an IBM Concert instance for visualization and management.

## Tasks Summary
- [**build_swi_collated**](build_swi_collated.yml) - Retrieve z/OS topology of software instances and their missing updates and CSI dataset information from zOSMF
- [**process_count**](process_count.yml) - Aggregate applied critical fixes for each FMID

## Set up job templates on Ansible Automation Platform
These playbooks are designed to be used with Ansible Automation Platform (AAP) job templates. The information in the `inventories` folder can be used to set up Inventory and Hosts on AAP. 

Make sure to check the variables in your `host_vars/zos_host.yml` file to make sure they are set to what you need. The `environment_vars` should not need to change.

There are also a variety of variables set at the playbook level `z_apars/send_data_to_concert.yml` at the top under `vars`, like the API key and Concert port.

To encrypt the z/OSMF password, use [ansible-vault](https://docs.ansible.com/ansible/2.8/user_guide/vault.html#ansible-vault) to encrypt_string and set the `vault_zmf_password` variable under `z_apars/inventories/host_vars/vault.yml`. Otherwise, prompt for it every time using an AAP survey or [vars_prompt](https://docs.ansible.com/ansible/latest/playbook_guide/playbooks_prompts.html) at the CLI.

Review the required inputs to each playbooks to set up Surveys on AAP so the job runs correctly.

- Set up an AAP [schedule](https://docs.ansible.com/automation-controller/latest/html/userguide/scheduling.html) to send cert data to IBM Concert on a regular basis
- Set up an AAP [job template](https://docs.ansible.com/automation-controller/latest/html/userguide/job_templates.html#create-a-job-template) to send data to Concert
- A survey is needed for the following playbook variables:
  ```
  zmf_host: ''
  zmf_port: ''
  zmf_user: ''
  zmf_password: ''
  izsam_jcl: ''
  izsam_csv_dsn: ''
  concert_url: ''
  concert_port: ''
  concert_api_key: ''
  concert_drupal_key: ''
  concert_api_token: ''
  concert_instance_id: ''
  concert_user: ''
  concert_password: ''
  ```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2025

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](../../../README.md#support) for more
details.