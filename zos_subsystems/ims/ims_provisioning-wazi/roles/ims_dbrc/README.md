DBRC Services
=========

This role defines DBRC's default settings and prepares DBRC.

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


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

© Copyright IBM Corporation 2022

License
-------

Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


