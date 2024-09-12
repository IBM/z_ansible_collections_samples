find_cert_info
=========

Extract certificate details

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **cert_serial**

  Specifies the cert serial number

- ### **app_port**

  Specifies the application port number

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
      name: find_cert_info
    vars:
      cert_serial: '56'
      app_port: '19080'
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
