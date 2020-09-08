# Copy Directory to PDS, Edit member and Submit Job

This sample playbook demonstrates how to copy a directory to a PDS, edit a 
member within the PDS and submit the PDS member containing JCL. This sample
uses the following modules to accomplish this use case:
- zos_copy
- zos_lineinfile
- zos_job_submit

## Getting Started

If you are unfamiliar with playbooks, you can review our
[detailed configuration guide](../../../docs/share/configuration_guide.md) or
continue with getting started below.

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
ansible-playbook -i inventory.yml copy_edit_submit.yml
```

# Copyright

© Copyright IBM Corporation 2020

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0).

# Support

All IBM certified sample playbooks, roles and filters are supported as part of
the Red Hat® Ansible Certified Content for IBM Z offering. Support for samples
is managed through the repositories git issues:
https://github.com/IBM/z_ansible_collections_samples/issues