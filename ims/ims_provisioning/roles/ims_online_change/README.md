Online Change Service
=========

This role enables IMS Online Change service (OLC).

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **enable_olc**

  If true, enable IMS Online Change service.

- ### **active_libs**

  If true, send active libraries to zOS



Dependencies
------------

None

Example Playbook:
----------------

```yaml 
        - include_role:
            name: ims_online_change
          vars:
            enable_olc: true
            active_libs: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


