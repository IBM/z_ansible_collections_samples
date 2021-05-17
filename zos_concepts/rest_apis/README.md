# Ansible z/OSMF URI Sample
This sample playbook demonstrates basic use cases for invoking a REST API service,
particularly the z/OSMF REST API services.


It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.


## Prerequisites

* Access to the [z/OSMF REST API](https://www.ibm.com/docs/en/zos/2.4.0?topic=guide-using-zosmf-rest-services)


## Ansible Collection Requirement

* IBM z/OS core collection 1.2.0 or later

## Playbook Summary

- [**uri-sample.yml**](uri-sample.yml) - Provides an example of how to invoke the z/OSMF REST API services with Ansible.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](https://github.com/IBM/z_ansible_collections_samples/blob/master/zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. It should
be noted that when you use the **host_setup** it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

### Update [inventories/zos_host](inventories/zos_host) with the information about your system(s)

```yaml
# the system where the data should be copied to
source_system:
  hosts:
    zos_host:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### Update the environment variables for each z/OS system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
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
### Run desired playbook

If `ansible-vault` was used to encrypt any sensitive information, run the following command.  
**Note:** This command will prompt you for the password used to encrypt the sensitive data.
```bash
ansible-playbook -i inventories/zos_host --ask-vault-pass uri-sample.yml
```

Otherwise, run the following command.
```bash
ansible-playbook -i inventories/zos_host uri-sample.yml
```

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.