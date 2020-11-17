Data Set Services
=========

Allocates all required data sets for IMS provisioning and de-allocates them for de-provisioning.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`

Role Variables
--------------
- ### **work_dataset**

  Allocate zCloud data sets
  - 'allocate': allocate zCloud dataset

- ### **sysdef_datasets**

  Specifies what action to take for IMS system definition data sets:
  - 'allocate': allocate system definition data sets
  - 'delete': delete system definition data sets

- ### **runtime_datasets**

  Specifies what action to take for IMS runtime data sets:
  - 'allocate': allocate runtime data sets
  - 'delete': delete runtime data sets



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

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


