list-load
===============

Load a file with libraries to be added, find out the missing libraries from the current APF list.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- `apf_list`: A list of current entries in the z/OS system APF list in JSON format returned from zos_apf Ansible module.

- `file_name`: The file containig the libraries to be added to APF list. (i.e ../files/list.bom )

Example Playbook
----------------

```yaml
- hosts: zos_host
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Load the list from the file in git repo and find out the delat from current APF list
      include_role:
        name: list-load
```

License
-------

Copyright (c) IBM Corporation 2020
Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Behnam Al Kajbaf behnam@ibm.com, [@balkajbaf](https://github.com/balkajbaf)

Support
-------

All IBM certified sample playbooks, roles and filters are supported as part of
the Red Hat® Ansible Certified Content for IBM Z offering. Support for samples
is managed through the repositories git issues:
https://github.com/IBM/z_ansible_collections_samples/issues
