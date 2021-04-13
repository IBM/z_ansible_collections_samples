# Manage Dynamic Resources in IBM Z System Automation Using Ansible

This project provides sample playbooks which can be used to create and remove dynamic resources managed by System Automation.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.

## Playbook Summary

- [**create-dynamic-resource.yaml**](create-dynamic-resource.yaml) - Create and resume a dynamic resource.
- [**delete-dynamic-resource.yaml**](delete-dynamic-resource.yaml) - Delete a dynamic resource.

## Ansible Collection Requirement

   IBM Z System Automation collection 1.0.0 or later

## Getting Started

You can use the sample [inventory](inventory.yaml), [hosts file](host_vars/zos_host.yaml), and [vars file](vars/vars.yaml)
to discover and create your system specific **inventory**, **host_vars**, and **vars** artifacts.

### 1. [inventory.yml](inventory.yml) simply points to the host vars file named zos_host.yaml

```yaml
sample:
  hosts:
    zos_host:
```

* **sample:** An example of host grouping.
  * **zos_host:** Nickname for the target z/OS system.


### 2. Update the settings for the z/OS system in [host_vars/zos_host.yaml](host_vars/zos_host.yaml)

You can supply host variables in either the inventory file or the separate variable file.

```yaml
# The value of this property identifies the host name of the IBM Z System Automation Operations REST server.
sa_service_hostname: your.host.name

# The value of this property identifies the port number of the IBM Z System Automation Operations REST server.
sa_service_port: port_number

# The value (http or https) of this property identifies if you have configured your IBM Z System Automation Operations REST server to use SSL.
sa_service_protocol: https
```

### 3. Update the playbook specific variables in [vars/vars.yaml](vars/vars.yaml) based on desired behavior

The sample **vars.yaml** contains the properties to identify the dynamic resource to create or delete.

```yaml
templateName: name_of_the_template
subsystem: subsystem_name
system: system_name
job: jobname
procedure: procedureName
comment: comment
group: group
sdesc: "a short description"
```

* **templateName**: The value of this property specifies the template name that will be used to create the dynamic resource. This parameter is mandatory.
* **subsystem**: The value of this property specifies the subsystem name of the new resource. This parameter is mandatory.
* **system**: The value of this property specifies the system where the resource will be created. This parameter is mandatory.
* **job**: The value of this property specifies the job name of the new resource. This parameter is mandatory.
* **procedure**: The value of this property specifies the procedure name used by the new resource. This parameter is optional.
* **comment**: The value of this property specifies a comment to be associated with the creation of the new resource. This parameter is optional.
* **group**: The value of this property specifies the automation name of the application group (APG) that will host the new resource. This parameter is optional.
* **sdesc**: The value of this property specifies a short description of the new resource. This parameter is optional.

### 4. Run desired playbook

```bash
ansible-playbook -i inventory.yml <playbook-name>
```

You are prompted for your NetView credentials during execution of the playbook. The environment variables "username_value"
and "password_value" are taken as default values for the credentials.

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](../../../README.md#support) for more
details.







