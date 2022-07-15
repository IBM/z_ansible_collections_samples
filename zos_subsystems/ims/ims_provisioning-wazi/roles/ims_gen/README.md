Gen Services
=========

This role provides DBD, PSB and ACB services

Requirements
------------

* Ansible Collection: ibm.ibm_zos_core and ibm.ibm_zos_ims


Role Variables
--------------

- ### **dbd_gen**

  When dbd_gen = true, prepares DBDGEN member list.

- ### **psb_gen**

  When psb_gen = true, prepares PSBGEN member list.

- ### **acb_gen**

  When acb_gen = true, builds ACB library.


Dependencies
------------

None

Example Playbook:
----------------

```yaml 
       - include_role:
            name: ims_gen
          vars:
            dbd_gen: true
            psb_gen: true
            acb_gen: true

```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


