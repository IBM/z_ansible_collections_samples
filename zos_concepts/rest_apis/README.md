# Ansible z/OSMF URI Sample
This sample playbook demonstrates basic use cases for invoking a REST API service,
particularly the z/OSMF REST API services.


It is a good practice to review the playbook contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Prerequisites

* Access to the [z/OSMF REST API](https://www.ibm.com/docs/en/zos/2.4.0?topic=guide-using-zosmf-rest-services)

## Playbook Requirements
This playbook requires:

- [IBM® z/OS® core collection 1.2.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)
- [Ansible® 2.9 or 2.11](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

Note, only the playbook requirements are listed in this document, please review
the collections documentation for additional requirements.

## Playbook Summary

- [**uri-sample.yml**](uri-sample.yml) - Provides an example of how to invoke the z/OSMF REST API services with Ansible.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

### Update the included [inventory](inventory) with the information about your system's.
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

### Update the playbook's [group_vars](group_vars/all.yml) with the corresponding values

These variables are used within the `uri-sample` playbook and are required for using the z/OSMF REST API.

```yaml
# z/OSMF host and port information
ZOSMF_HOST: zosmf_target_host
ZOSMF_PORT: zosmf_target_port

# Desired destination folder on target host for holding definition file
ZOSMF_DEST_FOLDER: /path/to/folder

# Credentials to access z/OSMF
ZOSMF_USER: user
ZOSMF_PASS: pass

# Nickname of the system on which the workflow is to be created.
# Use the nickname that is specified for the system definition
# in the z/OSMF Systems task.
ZOSMF_SYSTEM: zosmf_system
```

### Walkthrough of uri-sample.yml

The purpose of the `uri-sample` playbook is to showcase the ability to invoke a REST API service as well as give an example of creating, starting, and deleting workflows using the z/OSMF REST API service.


This playbook will use the following operations:
* Querying existing workflows
* Creation of a workflow
* Starting a workflow
* Cancellation of a workflow
* Deletion of a workflow

**Note:** to learn more about z/OSMF workflow services, visit [IBM Docs](https://www.ibm.com/docs/en/zos/2.4.0?topic=services-zosmf-workflow)

<br>

The `uri-sample` playbook starts by copying over the [sample_definition_file.xml](files/sample_definition_file.xml) (required for z/OSMF REST API service utilization) to the specified target folder, and has two outcomes after execution.
* Running the playbook the **first** time will result in
    * Creation of the **sample_workflow**
    * Starting the **sample_workflow**
* Running the playbook the **second** time will result in
    * Cancellation of the **sample_workflow**
    * Deletion of the **sample_workflow**
### Using Ansible Vault to Encrypt Passwords or Sensitive Data

The `ZOSMF_PASS` variable is stored in plaintext as shown above in the *group_vars/all.yml* file. If desired, the `ZOSMF_PASS` can be encrypted in several different ways; we will walk through one of those ways.

For this short tutorial, we will encrypt the following variable.
```yaml
ZOSMF_PASS: mysecretpassword
```

In order to encrypt the password, we will run the following `ansible-vault` command in our terminal.
```bash
ansible-vault encrypt_string mysecretpassword --ask-vault-pass
```

This command will prompt you to enter a password that will be used to decrypt the sensitive information while running the playbook.
After successfully selecting a password to encrypt the sensitive information, the command will produce a result similar to the following output.
```
!vault |
  $ANSIBLE_VAULT;1.1;AES256
  64313462656366313466633039373238663035653332366664663839373564613237636662663766
  3835383763393630366662366630633965396234303438660a373232633133646535336530353961
  38393864396132353637323837656336306633663464633964316637666661363931646565616435
  6332316134666538300a393833663835343134393466343265356337633637346631653761653230
  6365
Encryption successful
```

The only thing left to do is replace the plaintext password with this encrypted string.

```yaml
ZOSMF_PASS: !vault |
  $ANSIBLE_VAULT;1.1;AES256
  64313462656366313466633039373238663035653332366664663839373564613237636662663766
  3835383763393630366662366630633965396234303438660a373232633133646535336530353961
  38393864396132353637323837656336306633663464633964316637666661363931646565616435
  6332316134666538300a393833663835343134393466343265356337633637346631653761653230
  6365
```

### Run the playbook
This project has included a `site.yml` playbook that serves as the master playbook
that provides additional prerequisite checks then it invokes the `uri-sample.yml`
playbook.

If you want to run the master playbook `site.yml` it will check that your environment
has the correct version of Ansible as well as the collection needed to execute
correctly. To run the master playbook, use command:

```bash
ansible-playbook -i inventories/zos_host site.yml
```

You can skip the prerequisite check and run the `uri-sample.yml` with
command:

```bash
ansible-playbook -i inventories/zos_host uri-sample.yml
```

If `ansible-vault` was used to encrypt any sensitive information, add the
following to your command: `--ask-vault-pass`. **Note:** This will prompt you
for the password used to encrypt the sensitive data. e.g,, run the following command:

```bash
ansible-playbook -i inventories/zos_host --ask-vault-pass uri-sample.yml
```

```bash
ansible-playbook -i inventories/zos_host --ask-vault-pass site.yml
```

# Changelog
All changes are maintained chronologically by date found in the
[changelog](changelog.yml).

# Copyright
© Copyright IBM Corporation 2021

# License
Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support
Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.