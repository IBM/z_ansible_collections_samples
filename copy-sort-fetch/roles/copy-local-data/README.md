copy-local-data
=========

Copy a file on the control node to a sequential data set on the remote z/OS host.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- `dest_tmp_data_set`: The name of the data set where the file should be copied to. 
It can be an existing sequential data set. If it does not exist, it will be created.

- `target_charset`: The default system character set of the target machine.

Example Playbook
----------------

```yaml
- hosts: source_system
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Copy local file to remote host
      include_role:
        name: copy-local-data
        public: yes
```

License
-------

Copyright (c) IBM Corporation 2020
Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Asif Mahmud asif.mahmud@ibm.com, [@asifmahmud](https://github.com/asifmahmud)
