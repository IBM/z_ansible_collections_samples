# Ansible Db2 application discovery

This project provides the required sample playbooks and roles to discover and prompt all registered applications<sup>[1](#footnote1)</sup> that can be instantiated.

The execution of this playbook will return information for each registered applications under DOE, such as:
- subsystemIDs
- the set of db2Objects, such as:
  - database
  - tablespaces
  - indexes
  - views
  - stored procedures

It is a good practice to review the playbook sample contents before executing them. It will help you understand the requirements in terms of space, location, names, authority, and the artifacts that will be created and cleaned up. Although samples are written to operate without the need for the user’s configuration, flexibility is written into the samples because it is not easy to determine if a sample has access to the host’s resources. Review the playbook notes sections for additional details and configuration.

## Playbook Summary

- [**db2-discover-applications.yml**](db2-discover-applications.yml) - Handles retrieving the registered applications for a Db2 system.

## Role Summary

- [**db2_login**](roles/db2_login/README.md) - Holds tasks to authenticate the user of an application.
- [**db2_get_list_apps**](roles/db2_get_list_apps/README.md) - Holds tasks for retrieving the list of registered applications from a Db2 system.

## Before this task

These sample playbooks are designed to exploit the Db2 DevOps Experience (DOE) APIs. An environment where Db2 DevOps Experience is installed and operating is required.

In addition, the Db2 application discovery sample playbook can be applied after your administrator defines the Db2 subsystem, team, environment and the application, or the set of Db2 objects that can be provisioned as an unit under DOE. These information will be the input parameters for the playbook.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/zos_core/configuration_guide.md).

Update the playbook-specific variables in [vars/db2_discover_applications.yml](vars/db2_discover_applications.yml), based on the behavior that you want.

1. zos_target_address - Hostname of the system on which Db2 for z/OS DevOps Experience is running. Server name used to access the DOE rest apis.
2. valid_port_number - Port number of Db2 DevOps Experience Server.
3. valid_username - Username used to access the DOE REST API.
4. valid_password - Password used to access the DOE REST API.

## Run the playbook

1. To run the DB2 application discovery playbook, type the following command from the root of this repository:

  `ansible-playbook db2-discover-applications.yml`

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
