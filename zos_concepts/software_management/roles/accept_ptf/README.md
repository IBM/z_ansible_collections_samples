accept_ptf
=========

ACCEPT a PTF

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **dist_zone**

  Specifies the distribution zone name

- ### **ptf**

  Specifies the PTF to accept

- ### **global_csi**

  Specifies the CSI dataset

Example Playbook
----------------

```yaml
- hosts: zos_host
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"
  vars:

  roles:
    - role: accept_ptf
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
