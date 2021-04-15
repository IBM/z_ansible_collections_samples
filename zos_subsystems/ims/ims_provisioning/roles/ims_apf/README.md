Authorized Program Facility Service
=========

Authorize IMS datasets to z/OS.


Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`

Role Variables
--------------

- ### **auth_datasets**
  When auth_datasets is true, authorize IMS datasets to z/OS.


Dependencies
------------

None

Example Playbook:
----------------

```yaml
        - include_role:
            name: ims_apf
          vars:
            auth_datasets: true

```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

