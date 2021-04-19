query_csi
=========

Query CSI data

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------
Role Variables
--------------

- ### **zone**

  Specifies the zone name to perform the Query

- ### **ptf**

  Specifies the PTF to query

- ### **global_csi**

  Specifies the CSI dataset
- ### **list_operands**

  Specifies the LIST command operands

Example Playbook
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  roles:
    - role: query_csi
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
