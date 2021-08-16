add_certificate
=========

Add a digital certificate to a key ring and activate it

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **command**

  Specifies operator command to be issued
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

  - include_role:
      name: isssue_operator_cmd
    vars:
      command: 'd a,l'
      task_description: 'Display active users'
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
