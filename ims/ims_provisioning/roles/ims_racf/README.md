RACF Preparation
=========

Prepares RACF security for IMS.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **prep_racf**
  When prep_racf is true, this role prepares RACF security for IMS.


Dependencies
------------

None

Example Playbook:
----------------

```yaml 
        - include_role:
            name: ims_racf
          vars:
            prep_racf: true
```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


