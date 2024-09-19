get_cert_detail
=========

List a RACF certificate, extract certificate data and store it a dictionary, and add the dictionary into a list

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **role_cert**

  The certificate atrributes to be issued
- ### **tso_command**

  Specifies RACF command to be issued
- ### **task_description**

  Specifies a description related to the command to be issued


Example Playbook
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"
  vars:

  - include_role:
      name: get_cert_detail
    vars:
      role_cert: '{{cert_detail}}
      tso_command: 'RACDCERT SITE LIST(LABEL('{{cert_label}}'))'
      task_description: 'List a SITE certificate'
```

License
-------

Copyright (c) IBM Corporation 2024 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Andy Nguyen andy.nguyen@ibm.com

Copyright
---------

© Copyright IBM Corporation 2024
