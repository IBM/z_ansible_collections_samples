# Provision, configure, and start a Liberty instance

This project provides a sample playbook and roles which can be used to provision, configure, and start a basic [WebSphere Liberty server](https://www.ibm.com/cloud/websphere-liberty) instance.

It is a good practice to review the playbook sample contents before executing
them. It will help you understand the requirements in terms of space, location,
names, authority, and the artifacts that will be created and cleaned up.
Although samples are written to operate without the need for the user’s
configuration, flexibility is written into the samples because it is not easy
to determine if a sample has access to the host’s resources. Review the
playbook notes sections for additional details and configuration.

## Playbook Summary

- [**provision.yml**](provision.yml) - Handles the following tasks:
  - Creating and mounting a zSeries file system (zFS) for Open Multiple Virtual Storage (OMVS)
  - Creating a standard Liberty server instance with the option of custom configuration files
  - Enabling z/OS&reg; authorized services
  - Starting an angel process or a server process
- [**deprovision.yml**](roles/deprov/README.md) - Handles the following tasks:
  - Stopping an angel process or a server process
  - Removing a Liberty server directory
  - Unmounting a zFS file system
  - Deleting a zFS file system
  - Deleting JCL procedure templates

## Role Summary

- [**create_zFS**](roles/create_zFS/README.md)- Holds tasks to create and format a zFS file system.
- [**mount_zFS**](roles/mount_zFS/README.md)- Holds tasks to mount a zFS file system.
- [**create_server**](roles/create_server/README.md)- Holds tasks to create a Liberty server instance.
- [**configure_server**](roles/configure_server/README.md)- Holds tasks to transfer optional configuration files for a Liberty server.
- [**started_server**](roles/started_server/README.md)- Holds tasks to create System Authorization Facility (SAF) STARTED profiles for running the Liberty server process as a STARTED task.
- [**angel_config**](roles/angel_config/README.md)- Holds tasks to create a SAF STARTED profile for the Liberty angel process to run as a STARTED task, or to create a SAF SERVER profile to enable the Liberty server access to z/OS&reg; authorized services.
- [**authorized_services**](roles/authorized_services/README.md)- Holds tasks to enable optional z/OS&reg; authorized services.


## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](https://github.com/IBM/z_ansible_collections_samples/blob/master/docs/share/zos_core/configuration_guide.md) or
continue with getting started below.

Optionally, you can use the sample
[host_setup](https://github.com/IBM/z_ansible_collections_samples/blob/master/zos_administration/host_setup/README.md)
to discover and create your **inventory** and **host_vars** artifacts. Please
note that when you use the **host_setup**, it will generate a configuration
for the most common dependencies, some playbooks require more customized
configurations, in this case, you can review the sample documentation and
add the additional required variables.

1. Update [inventory.yml](inventory.yml) with the information about your systems

```yaml
zsystem:
  hosts:
    zos:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

2. Update the environment variables for the z/OS&reg; system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"

# the path to the root of JAVA installation
JAVA: "/usr/lpp/java170/J7.0_64"

# the path to user directory that contains the server instance configuration
WLP_USER_DIR: '/u/oeusr01'

# the path to the root of Liberty installation
liberty_path: '/usr/lpp/zWAS/WAS900/Liberty/V19R00'
```

3. Update custom server variables based on the desired behavior for the Liberty instance in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
CUSTOMIZE: no
ANGEL: no
AUTHORIZED_SERVICES: no

```

4. Customize the availability of authorized services on the z/OS&reg; system for the server
Within the `authorized_services` role, there are variables with boolean values to represent which z/OS&reg; authorized services to enable.
```yaml
SAFCRED: yes
ZOSWLM: yes
TXRSS: yes
ZOSDUMP: yes
LOCAL_ADAPTER: no
PRODMGR: no
ZOSAIO: no
```

5. Run playbook

> **_NOTE:_**  Running the commands with tags will have precedence over the conditionals within the playbook

To start the server and angel process (if enabled):
```bash
ansible-playbook -i inventory.yml provision.yml
```

To just start the server:
```bash
ansible-playbook -i inventory.yml provision.yml --skip-tags "start_angel"
```

To just stop the server and keep the angel process:
```bash
ansible-playbook -i inventory.yml deprovision.yml --skip-tags "stop_angel"
```

To just stop the server and angel process:
```bash
ansible-playbook -i inventory.yml deprovision.yml --tags "stop,stop_angel"
```

## Copyright
© Copyright IBM Corporation 2021

## License
Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

## Author Information
Stephanie Lieu - @stephanie-lieu or @steph-lieu

## Support
Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
