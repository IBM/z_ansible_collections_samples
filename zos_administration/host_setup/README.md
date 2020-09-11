# Configure Python and ZOAU Installation

This playbook is used to locate required dependencies (Python, ZOAU) on the
remote z/OS host and build a suitable host configuration using the discovered
dependencies.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Ansible Collection Requirement

   IBM z/OS core collection 1.0.0 or later

## Getting Started

This playbook would generate the [inventory](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md#inventory)
as well as the [host_vars](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md#variables).
If you are unfamiliar with playbooks, you can review
our [detailed configuration guide](../../docs/share/configuration_guide.md)

### Run desired playbook

```bash
ansible-playbook -i "mysystem.to.connect.com," host_setup.yaml
```

If the user that connects to the host is different from the user submitting
the playbook, you can optionally instruct the playbook to user another user
with option '-u <username>' for example:

```bash
ansible-playbook -i "mysystem.to.connect.com," host_setup.yaml -u myHostUser
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../README.md#support) for more
details.