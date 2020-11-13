Catalog Services
=========

This role allocates, loads and deletes all IMS catalogs.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`

Role Variables
--------------

- ### **catalog**

  Specifies what action to take:
  - 'allocate': allocate catalogs for IMS
  - 'load': load catalogs for IMS
  - 'delete': delete all catalogs


Example Playbook: How to allocate and load catalogs
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core
    - ibm.ibm_zos_ims

  gather_facts: no
  environment: "{{ environment_vars }}"

  - include_role:
      name: ims_catalog
    vars:
      catalog: "allocate"

  - include_role:
      name: ims_catalog
    vars:
      catalog: "load"
```

Example Playbook: How to delete catalogs
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core  
    - ibm.ibm_zos_ims
  gather_facts: no
  environment: "{{ environment_vars }}"

  - include_role:
      name: ims_catalog
    vars:
      catalog: "delete"
```


License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Jerry Li lij@us.ibm.com

Copyright
---------

© Copyright IBM Corporation 2020
