# Copy, Sort and Fetch Data Sets on z/OS using Ansible

This project provides sample playbooks and roles which is used to demonstrate 
how to use many of the common core Ansible modules included in Red Hat Ansible 
Certified Content for IBM Z. This playbook copies a local file containing tabular 
data to a sequential data set on the remote z/OS system. This data set is then sorted
based on a particular criteria and the sorted data set is fetched back to the
control node. The following core modules are used to accomplish these set of tasks:

- `zos_copy`
- `zos_lineinfile`
- `zos_mvs_raw`
- `zos_fetch`

Note that this playbook requires IBM z/OS Core Collection 1.3.0-beta.1 or later

## Getting Started

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

### Update the environment variables for each z/OS system in [host_vars/source.yml](host_vars/zos_host.yml)

```yaml
# the path to the root of IBM python installation
PYZ: "/usr/lpp/IBM/cyp/v3r8/pyz"

# the path to root of ZOAU installation
ZOAU: "/usr/lpp/IBM/zoautil"
```

### Run desired playbook

```bash
ansible-playbook -i inventory.yml copy-sort-fetch.yml
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).