issue_racf_cmd
=========

Issue RACF commands

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **command**

  Specifies the RACF command(s) to be issued
- ### **task_description**

  Specifies a description related to the command to be issued

Example Playbook
----------------

```yaml
- hosts: zos_host
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"
  vars:

  tasks:
  - include_role:
      name: issue_racf_cmd
    vars:
      task_description: 'Create a new keyring'
      command:
          - RACDCERT ADDRING(SharedRing1) ID({{owner_id}})
          - RACDCERT LISTRING(SharedRing1) ID({{owner_id}})
```

License
-------

Copyright (c) IBM Corporation 2021 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Andy Nguyen andy.nguyen@ibm.com

Copyright
---------

© Copyright IBM Corporation 2021
