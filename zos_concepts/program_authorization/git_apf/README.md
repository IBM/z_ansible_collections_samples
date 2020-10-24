# Sync APF authorized libraries on zO/S with a configuration file cloned from GitHub source control

This playbook clones a git repository to the desired directory on the zO/S system.
Loads a configuration file containing a set of libraries (data sets) that are
required to be present on the zO/S system APF authorized list. Later generates
a list of libraries to be added to APF authorized list based on missing libraries
from the current APF list. finally makes APF statement entries in the desired 
data set or data set member.

## Ansible Collection Requirement

   IBM z/OS core collection 1.2.0 or later

## zO/S Requirement Setup

    - `git` command 
    - ZOAU 1.1.0 or later

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
# the system where the data should be copied to
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

### Configuration file format

    <library> [volume]
    ...

```
APFTEST.PGRM001.LIB001 T60313
APFTEST.PGRM001.LIB002 T60314
APFTEST.PGRM001.LIB003 T60315
```

### Run desired playbook

```bash
ansible-playbook -i inventory.yml prog_auth.yml
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

Please refer to the [support section](../../../README.md#support) for more
details.