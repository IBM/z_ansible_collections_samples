DBRC Services
=========

Defines DBRC's default settings and prepares DBRC to record and manage information.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **dbrc_defaults**

  When dbrc_defaults = true, defines default settings for DBRC


- ### **prep_dbrc**

  When prep_dbrc = true, prepares DBRC


Dependencies
------------

None

Example Playbook:
----------------

```yaml
        - include_role:
            name: ims_dbrc
          vars:
            prep_dbrc: true

        - include_role:
            name: ims_dbrc
          vars:
            dbrc_defaults: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


