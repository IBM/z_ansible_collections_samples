restore-ptf
=========

RESTORE (back-out) a PTF

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **ptf**

  Specifies the PTF to accept

- ### **global_csi**

  Specifies the CSI dataset
- ### **reject_sysmod**

  Specifies `true` or `false` to reject the PTF from the Global zone after RESTORE

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
    - role: restore_ptf
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
