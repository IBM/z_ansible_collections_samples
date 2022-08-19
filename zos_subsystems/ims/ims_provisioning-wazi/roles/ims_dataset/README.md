Data Set Services
=========

This role allocates all required data sets for IMS provisioning and de-allocates them for de-provisioning.

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


Role Variables
--------------

| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DFS_AUTH_LIB_HQL1                | The first qualifier of library name, for example 'IMSTEST'                                                                                                                            |
| DFS_AUTH_LIB_HQL2                           | The second qualifier of library name, for example 'IMS1'       
| work_dataset                | If work_dataset = "allocate", zCloud dataset will be allocated.                                                                                                                            |
| sysdef_dataset                | If sysdef_dataset = "allocate", IMS System datasets will be allocated.  If  sysdef_dataset = "delete", these datasets will be deleted.                                                                                                                        |
| runtime_dataset                | If runtime_dataset = "allocate", IMS runtime libraries will be allocated.  If  runtime_dataset = "delete", these datasets will be deleted.                                                                                                                        |

                                                                                          

Dependencies
------------

None

Example Playbook
----------------

```yaml

       - include_role:
            name: ims_dataset
          vars:
            work_dataset: allocate
            sysdef_datasets: allocate
            runtime_datasets: allocate

```

## Copyright

© Copyright IBM Corporation 2022

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


