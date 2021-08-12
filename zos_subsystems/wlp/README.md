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

- [**provision.yml**](provision.yml) - Handles creating and mounting a zFS filesystem for OMVS, creating a standard Liberty server instance with the option of using custom configuration files, enabling z/OS authorized services, and starting the angel process and server process. 
- [**deprovision.yml**](roles/deprov/README.md) - Handles stopping an angel process and the server process, removing Liberty server directory, unmounting zFS filesystem, deleting the zFS filesystem, and deleting JCL procedure templates

## Role Summary

- [**create_zFS**](roles/create_zFS/README.md)- Holds tasks to create and format a zFS filesystem.
- [**mount_zFS**](roles/mount_zFS/README.md)- Holds tasks to mount the zFS filesystem.
- [**create_server**](roles/create_server/README.md)- Holds tasks to create a Liberty server instance.
- [**configure_server**](roles/configure_server/README.md)- Holds tasks to transfer optional configuration files for a Liberty server.
- [**started_server**](roles/started_server/README.md)- Holds tasks to create SAF STARTED profiles for running the Liberty server process as a STARTED task. 
- [**security**](roles/security/README.md)- Holds tasks to create a SAF STARTED profile for the Liberty angel process to run as a STARTED task, create SAF SERVER profile to enable the Liberty server access to z/OS authorized services, and enable optional z/OS authorized services.


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

# the path to the root of JAVA installation
JAVA: "/usr/lpp/java170/J7.0_64"

# the path to user directory that contains the server instance configuration
WLP_USER_DIR: '/u/oeusr01'

# the path to the root of Liberty installation
liberty_path: '/usr/lpp/zWAS/WAS900/Liberty/V19R00'
```
### 3. Update custom server variables based on desired behavior for the Liberty instance in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
CUSTOMIZE: no
SECURITY: no

```
Within the `security` role, there are variables with boolean values to represent which z/OS authorized services to enable.
```yaml
SAFCRED: yes
ZOSWLM: yes
TXRSS: yes
ZOSDUMP: yes
LOCAL_ADAPTER: no
PRODMGR: no 
ZOSAIO: no
```

### 4. Run playbook

To start the server and angel process (if enabled): 
```bash
ansible-playbook -i inventory.yml provision.yml 
```

To just start the server: 
```bash
ansible-playbook -i inventory.yml provision.yml --skip-tags "start_angel" 
```

To just stop the server and angel process: 
```bash
ansible-playbook -i inventory.yml deprovision.yml --tags "stop,stop_angel" 
```
