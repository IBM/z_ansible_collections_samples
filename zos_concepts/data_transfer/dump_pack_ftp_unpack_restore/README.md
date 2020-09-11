# Transfer Data Sets Using Ansible

This project provides sample playbooks and roles which can be used to transfer data sets between z/OS systems.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**transfer-data-sets.yml**](transfer-data-sets.yml) - Handles full transfer process: dump, pack, FTP, unpack and restore.
- [**dump-pack-ftp.yml**](dump-pack-ftp.yml) - Handles transfer of data sets: dump, pack and FTP.
- [**unpack-restore**](unpack-restore.yml) - Handles unpack and restore of data sets from a terse of a data set dump.

## Role Summary

- [**dump-pack-ftp-data-sets**](roles/dump-pack-ftp-data-sets/README.md) - Holds tasks related to transfer of data sets: dump, pack and FTP.
- [**unpack-restore-data-sets**](roles/unpack-restore-data-sets/README.md) - Holds tasks related to unpack and restore of data sets from a terse of a data set dump.
- [**get-estimated-size-of-data-sets**](roles/get-estimated-size-of-data-sets/README.md) - Estimates the total amount of storage space used by one or more data sets.

## Ansible Collection Requirement

   IBM z/OS core collection 1.2.0 or later

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

### Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
# the system which initially holds the data sets
source_system:
  hosts:
    source:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
# the system which should receive a copy of the data sets on source_system
destination_system:
  hosts:
    destination:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### Update the environment variables for each z/OS system in [host_vars/source.yml](host_vars/source.yml) and [host_vars/destination.yml](host_vars/destination.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Update the variables in [host_vars/source.yml](host_vars/source.yml) and [host_vars/destination.yml](host_vars/destination.yml) based on desired operations

### Run desired playbook

```bash
ansible-playbook -i inventory.yml <playbook-name>
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)

# Support

Please refer to the [support section](../../../README.md#support) for more
details.
