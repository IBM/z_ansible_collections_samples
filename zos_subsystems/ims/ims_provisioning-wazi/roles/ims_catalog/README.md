Role Name
=========

This role performs tasks related to catalog depending on `catalog` variable value (see details 
in role variable section).  The following sequence can be followed to start up catalog service:
1. Allocate
2. Setup
3. Load

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


Role Variables
--------------

* ansible_user: This user ID (used to log in the USS) is passed to this role.
* catalog:
  - "allocate": allocate datasets required by IMS catalog
  - "load": load the catalog
  - "update": update the catalog
  - "setup": create ACB for catalog
  - "delete": delete the catalog

Dependencies
------------

None

Example Playbook
----------------

This role can be included in another role as the following:

    - include_role:
        name: ims_catalog
      vars:
        catalog: allocate


## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).

