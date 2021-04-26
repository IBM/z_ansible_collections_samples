order_ptf
=========

Order a PTF from IBM.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **ptf**

  PTF number to be ordered
- ### **global_csi**

  Specifies the CSI dataset
- ### **smpe_root**

  Specifies the smpnts directory path to download the order packages
- ### **server_info**

  Specifies server info for SMPE RECEIVE ORDER command
- ### **client_info**

  Specifies client info for SMPE RECEIVE ORDER command

Example Playbook
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  roles:
    - role: order_ptf
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
