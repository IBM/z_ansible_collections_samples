# Provision and configure a basic zCEE instance

This project provides sample playbooks and roles which can be used to provision and configure a basic [**z/OS Connect EE**](https://www.ibm.com/products/zos-connect-enterprise-edition) instance.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**provision-zcee.yml**](provision-zcee.yml) - Handles reserving ports, creating and mounting zFS filesystem for OMVS, creating z/OS Connect server from the template, configure artifacts, define keyring, define security and start the server. 
- [**deprovision-zcee.yml**](deprovision-zcee.yml) - Handles stopping the server instance that is running.

## Role Summary

- [**zcee_configure_artifacts**](roles/zcee_configure_artifacts/README.md) - Holds tasks to configure and deploy the artifacts.
- [**zcee_create_format_zfs**](roles/zcee_create_format_zfs/README.md) - Holds tasks to create and format zFS filesystem.
- [**zcee_reserve_ports**](roles/zcee_reserve_ports/README.md) - Holds tasks for reserving the ports.
- [**zcee_mount_zfs**](roles/zcee_mount_zfs/README.md) - Holds tasks for mounting the zFS filesystem.
- [**zcee_zcon_setup**](roles/zcee_zcon_setup/README.md) - Holds tasks to run the zcon install command.
- [**zcee_create_server**](roles/zcee_create_server/README.md) - Holds tasks to create z/OS Connect EE server based the z/OS Connect EE template
- [**zcee_keyring**](roles/zcee_keyring/README.md) - Holds tasks to define a z/OS Connect EE keyring.
- [**zcee_security**](roles/zcee_security/README.md) - Holds tasks to define a z/OS Connect EE security.
- [**zcee_start_server**](roles/zcee_start_server/README.md) - Holds tasks to start a z/OS Connect EE server.
- [**zcee_stop_server**](roles/zcee_stop_server/README.md) - Holds tasks to stop a z/OS Connect EE server. 

## Ansible Collection Requirement

   IBM z/OS core collection 1.2.0 or later

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](https://github.com/IBM/z_ansible_collections_samples/blob/master/zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. Please
note that when you use the **host_setup**, it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

## Getting Started: CLI

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/configuration_guide.md) or
continue with getting started below.

### 1. Update [inventories/zvm](inventories/zvm) with the information about your system(s)

```yaml
zsystem:
  hosts:
    zos:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### 2. Update the environment variables for the z/OS system in [host_vars/zvm.yml](host_vars/zvm.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### 3. Update the playbook specific variables in [vars/zcee.yml](vars/zcee.yml) based on desired behavior

### 4. Run desired playbook

```bash
ansible-playbook -i inventories/zos_host <playbook-name>
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more
details.

