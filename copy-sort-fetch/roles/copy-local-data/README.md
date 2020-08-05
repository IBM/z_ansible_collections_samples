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

Dependencies
------------

A list of other roles hosted on Galaxy should go here, plus any details in regards to parameters that may need to be set for other roles, or variables that are used from other roles.

Example Playbook
----------------

Including an example of how to use your role (for instance, with variables passed in as parameters) is always nice for users too:

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

Copyright
-------

© Copyright IBM Corporation 2020
