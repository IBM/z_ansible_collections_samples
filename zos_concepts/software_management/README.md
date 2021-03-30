# Apply PTFs using Using Ansible

This project provides sample playbooks and roles which can be used to perform SMPE operations.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**add-user.yml**](add-user.yml) - Handles adding a user to z/OS. Playbook includes: group configuration, granting permissions, generating passwords, creating and mounting ZFS filesystem for OMVS, transferring files and templates, creating generic profile, and creating catalog alias. Playbook is designed to be used standalone or in an Ansible Tower workflow template.
- [**remove-user.yml**](remove-user.yml) - Handles removal of a user from z/OS. Playbook includes removal of all configuration performed in **add-user.yml**. Playbook is designed to be used standalone or in an Ansible Tower workflow template.
- [**send-rejection-email.yml**](send-rejection-email.yml) - Handles sending rejection email in the event a user's request for a new UserID is rejected. Playbook is designed to be used in an Ansible Tower workflow template.

## Role Summary

- [**add-zos-user**](roles/add-zos-user/README.md) - Holds tasks related to adding a z/OS user.
- [**remove-zos-user**](roles/remove-zos-user/README.md) - Holds tasks related to removing a z/OS user.

## Ansible Collection Requirement

   IBM z/OS core collection 1.3.0 or later

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](../../../zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. It should
be noted that when you use the **host_setup** it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

## Getting Started: CLI

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

### 1. Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
zsystem:
  hosts:
    zos:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### 2. Update the environment variables for the z/OS system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### 3. Update the playbook specific variables in [host_vars/zos_host.yml](host_vars/zos_host.yml) based on desired behavior

### 4. Run desired playbook

```bash
ansible-playbook -i inventory.yml <playbook-name>
```

## Getting Started: Ansible Tower

Please refer to the [Tower setup documentation](tower-setup.md) for a step-by-step guide for getting user management configured in Ansible Tower.

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](../../../README.md#support) for more
details.
