Catalog Services
=========

This role allocates, loads and deletes all IMS catalogs.

Requirements
------------

* Ansible Collection: ibm.ibm_zos_core and ibm.ibm_zos_ims

Role Variables
--------------

| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DFS_AUTH_LIB_HQL1                | The first qualifier of library name, for example 'IMSTEST'                                                                                                                            |
| DFS_AUTH_LIB_HQL2                           | The second qualifier of library name, for example 'IMS1'       
| catalog                | If catalog = "allocate", catalogs will be allocated.  If catalog = "load", catalogs will be loaded.  If catalog = "delete", catalogs will be deleted                                                                                                                          |
                                                                                          

Dependencies
------------

None

Example Playbook: How to allocate and load catalog
----------------

```yaml
        - include_role:
            name: ims_catalog
          vars:
            catalog: allocate
 
         - include_role:
            name: ims_catalog
          vars:
            catalog: load

```

Example Playbook: How to delete catalog
----------------

```yaml
        - include_role:
            name: ims_catalog
          vars:
            catalog: delete
 
```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


