# Ansible Discovery of Db2 subsystem

This project provides the required sample playbook and roles to discover the Db2 subsystem information and prompt the information on all registered Db2 subsystems.

The execution of this playbook will return information for each registered Db2 subsystem under DOE, such as:
- groupName
- isActive
- listenerPort
- location
- sysplexName
- systemName
- version

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**db2-discover-subsystems.yml**](db2-discover-subsystems.yml) - Handles discovery of all registered Db2 subsystems.

## Role Summary

- [**db2_login**](roles/db2_login/README.md) - Holds tasks to authenticate the user of an application.
- [**db2_post_graphql_discovery**](roles/db2_post_graphql_discovery/README.md) - Holds tasks to discover a Db2 subsystem.
- [**db2_put_infra_discovery**](roles/db2_put_infra_discovery/README.md) - Holds tasks to discover the infrastructure details of a Db2 subsystem.

## Before this task

These sample playbooks are designed to exploit the Db2 DevOps Experience (DOE) APIs. An environment where Db2 DevOps Experience is installed and operating is required.

In addition, the Db2 discovery sample playbook can be applied after your administrator defines the Db2 subsystem, team, environment and the application, or the set of Db2 objects that can be provisioned as an unit under DOE. These information will be the input parameters for the playbook.

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/zos_core/configuration_guide.md) or
continue with getting started below.

### Getting Started: CLI

A few settings may need to be changed to ensure compatibility with your z/OS target.

For more information on Python configuration requirements on z/OS, refer to [Ansible FAQ: Running on z/OS](https://docs.ansible.com/ansible/latest/reference_appendices/faq.html).

Update the playbook-specific variables in [vars/db2_discover_subsystems.yml](vars/db2_discover_subsystems.yml), based on the behavior that you want.


## Run the playbook

1. To run the DB2 discovery playbook, type the following command from the root of this repository:

  `ansible-playbook db2-discover-subsystems.yml`


# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.
