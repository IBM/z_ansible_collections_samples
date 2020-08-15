# Manage z/OS Users Using Ansible

This project provides sample playbooks and roles which can be used to create and remove users from a z/OS system.

## Playbook Summary

- [**add-user.yml**](add-user.yml) - Handles adding a user to z/OS. Playbook includes: group configuration, granting permissions, generating passwords, creating and mounting ZFS filesystem for OMVS, transferring files and templates, creating generic profile, and creating catalog alias.
- [**remove-user.yml**](remove-user.yml) - Handles removal of a user from z/OS. Playbook includes removal of all configuration performed in **add-user.yml**.

## Role Summary

- [**add-zos-user**](roles/add-zos-user/README.md) - Holds tasks related to adding a z/OS user.
- [**remove-zos-user**](roles/remove-zos-user/README.md) - Holds tasks related to removing a z/OS user.

## Getting Started

### Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
zsystem:
  hosts:
    zos:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### Update the environment variables for the z/OS system in [host_vars/zhost.yml](host_vars/zhost.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Update the variables in [roles/add-zos-user/defaults/main.yml](roles/add-zos-user/defaults/main.yml) and [roles/remove-zos-user/defaults/main.yml](roles/remove-zos-user/defaults/main.yml) based on desired operations

### Run desired playbook

```bash
ansible-playbook -i inventory.yml <playbook-name>
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).
