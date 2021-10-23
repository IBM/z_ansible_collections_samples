# Ansible Db2 schema provisioning

This project provides the required sample playbooks and roles to create the new instance of an application<sup>[1](#footnote1)</sup> to provision Db2 schema.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**db2-schema-provision.yml**](db2-schema-provision.yml) - Handles creating an instance of an application with/without data and retrieving the status of a newly created application instance.
- [**db2-schema-deprovision.yml**](db2-schema-deprovision.yml) - Handles deleting the instance of an application.

## Role Summary

- [**db2_login**](roles/db2_login/README.md) - Holds tasks to authenticate the user of an application.
- [**db2_get_app_id**](roles/db2_get_app_id/README.md) - Holds tasks for retrieving the ID of an application.
- [**db2_get_status_instance**](roles/db2_get_status_instance/README.md) - Holds tasks for retrieving the status of an application instance.
- [**db2_get_subsystem_id**](roles/db2_get_subsystem_id/README.md) - Holds tasks for retrieving the subsystem ID.
- [**db2_get_team_id**](roles/db2_get_team_id/README.md) - Holds tasks for retrieving the team ID and environment ID.
- [**db2_post_create_instance**](roles/db2_post_create_instance/README.md) - Holds tasks to create a new instance of an application.
- [**db2_delete_app_instance**](roles/db2_delete_app_instance/README.md) - Holds tasks to delete an application instance.

## Before this task

These sample playbooks are designed to exploit the Db2 DevOps Experience (DOE) APIs. An environment where Db2 DevOps Experience is installed and operating is required.

In addition, the Db2 schema provisioning sample playbook can be applied after your administrator defines the Db2 subsystem, team, environment and the application, or the set of Db2 objects that can be provisioned as an unit under DOE. These information will be the input parameters for the playbook.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](https://github.com/IBM/z_ansible_collections_samples/blob/master/zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. Please
note that when you use **host_setup**, it generates a configuration
for the most common dependencies. Some playbooks require more customized
configurations; in this case, you can review the sample documentation and
add the additional required variables.

### Getting Started: CLI

A few settings may need to be changed to ensure compatibility with your z/OS target.

For more information on Python configuration requirements on z/OS, refer to [Ansible FAQ: Running on z/OS](https://docs.ansible.com/ansible/latest/reference_appendices/faq.html).


1. Update [inventories/native](inventories/native) to contain the information needed to connect to the target. The following inputs are required:
     * ansible_host: an IP or a URL to the target
     * ansible_user: the username used to login with SSH
     * ansible_python_interpreter: the path on the target to the Python interpreter

 Below is an example where `zsystem` will be the name used to reference the target:

    ```yaml
    zsystem:
      hosts:
        native:
          ansible_host: zos_target_address
          ansible_user: zos_target_username
          ansible_python_interpreter: /var/hamb380/usr/lpp/IBM/cyp/v3r8/pyz/bin/python3.8
    ```

2. Update the environment variables for the z/OS system in [host_vars/native.yml](host_vars/native.yml)

  ```yaml
  # the path to the root of IBM python installation
  PYZ: "/SYSTEM/var/hamb380/usr/lpp/IBM/cyp/v3r8/pyz"

  ```

3. Update the playbook-specific variables in [vars/db2_schema.yml](vars/db2_schema.yml), based on the behavior that you want.


### Run the playbook

1. To run the DB2 schema provisioning playbook, type the following command from the root of this repository:

  `ansible-playbook -i inventories/native db2-schema-provision.yml`

2. To run the de-provisioning DB2 schema, type the following command from the root of this repository:

  `ansible-playbook -i inventories/native db2-schema-deprovision.yml`

### Note: 
<sup>[1](#footnote1)</sup> An application is a set of objects, such as tablespaces, tables, and indexes that are grouped to be managed and provisioned as a single unit for the use of an application program or a set of application programs. Application objects are logical, which means that they are only references to the objects. When users provision instances of an application, the referenced objects are copied to create the instances.

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.
