# Manage z/OS Users Using Ansible

This project provides sample playbooks and roles which can be used to create and remove users from a z/OS system.

## Playbook Summary

- [**add-user.yml**](add-user.yml) - Handles adding a user to z/OS. Playbook includes: group configuration, granting permissions, generating passwords, creating and mounting ZFS filesystem for OMVS, transferring files and templates, creating generic profile, and creating catalog alias. Playbook is designed to be used standalone or in an Ansible Tower workflow template.
- [**remove-user.yml**](remove-user.yml) - Handles removal of a user from z/OS. Playbook includes removal of all configuration performed in **add-user.yml**. Playbook is designed to be used standalone or in an Ansible Tower workflow template.
- [**send-rejection-email.yml**](send-rejection-email.yml) - Handles sending rejection email in the event a user's request for a new UserID is rejected. Playbook is designed to be used in an Ansible Tower workflow template.

## Role Summary

- [**add-zos-user**](roles/add-zos-user/README.md) - Holds tasks related to adding a z/OS user.
- [**remove-zos-user**](roles/remove-zos-user/README.md) - Holds tasks related to removing a z/OS user.

## Getting Started

### 1. Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
zsystem:
  hosts:
    zos:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### 2. Update the environment variables for the z/OS system in [host_vars/zhost.yml](host_vars/zhost.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### 3. Update the playbook specific variables in [host_vars/zhost.yml](host_vars/zhost.yml) based on desired behavior

### 4. Run desired playbook

```bash
ansible-playbook -i inventory.yml <playbook-name>
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).
