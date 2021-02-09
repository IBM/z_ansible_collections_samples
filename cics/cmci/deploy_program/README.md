# Using the CICS and z/OS collections to deploy a program to a CICS region

This sample playbook demonstrates how to use the `zos_copy` module from the `ibm_zos_core` collection in
conjunction with the `cmci_action` module from the `ibm_zos_cics` collection.

This sample shows how to use these modules to deploy a load module from a build output dataset to a CICS
load library, and `NEWCOPY` the `PROGRAM` in CICS.

This sample additionally shows how to automate installation of pre-requisites for the `cmci_*` modules.

## Requirements
   - Python 2.7+
   - Ansible 2.9+
   - IBM z/OS CICS Ansible collection 1.0.0+
   - IBM z/OS Core Ansible collection 1.2.0+

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

###  Update [inventory.yml](inventory.yml) with the information about your system(s)

```yaml
# the target z/OS system
source_system:
  hosts:
    zos_host:
      ansible_host: zos_target_address
      ansible_user: zos_target_username
      ansible_python_interpreter: path_to_python_interpreter_binary_on_zos_target
```

### Update the environment variables for each z/OS system in [host_vars/zos_host.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Update the host variables for which CICS PROGRAM to deploy

```yaml
# Target CMCI hostname
cmci_host: example.com

# Target CMCI port
cmci_port: 12345

# CMCI scheme (http or https)
scheme: https

# Target CICSplex SM context
context: MYPLEX

# Target CICSplex SM scope (probably the target region name)
scope: MYRGN

# Name of the target program
program: PRG1

# Name of the dataset containing the build output load module
build_ds: BLD.OUTPUT

# Name of the destination load library for the load module
load_lib: CICS.LLIB

# CMCI user name (leave blank for unauthenticated)
cmci_user:

# CMCI password (leave blank for unauthenticated)
cmci_password:
```

### Run [deploy_program.yml](deploy_program.yml)

```bash
ansible-playbook -i inventory.yml deploy_program.yml
```

# Copyright

© Copyright IBM Corporation 2021

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../../README.md#support) for more
details.