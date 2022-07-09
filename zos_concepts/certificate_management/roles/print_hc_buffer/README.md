print_hc_buffer
=========

Extract Health Checker data

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **hc_check**

  Specifies the Health Checker check to print the data for

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
      name: print_hc_buffer
    vars:
      hc_check: 'IBMRACF,RACF_CERTIFICATE_EXPIRATION'
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
