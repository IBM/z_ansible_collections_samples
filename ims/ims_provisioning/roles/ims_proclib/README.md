IMS Proclib operations
=========

Creates a configuration file in PROCLIB and copies PROCLIB members.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **add**
  When add is true, this role adds required members into PROCLIB.

- ### **copy**
  When copy is true, this role copies members from PROCLIB to zCloud PROCLIB.

- ### **copy_stage_libs**
  When copy_stage_libs is true, this role copies staging library to active library.

- ### **delete**
  When delete is true, this role deletes all members in zCloud PROCLIB.

- ### **create_sample**
  When create_sample is true, this role creates sample JCLs in PROCLIB.


Dependencies
------------

None

Example Playbook: Create configuration files for OM
----------------

```yaml 
        - include_role:
            name: ims_operations_manager
          vars:
            om: "config"
```

Example Playbook: Start OM
----------------

```yaml 
        - include_role:
            name: ims_operations_manager
          vars:
            om: "start"
```

Example Playbook: Stop OM
----------------

```yaml 
        - include_role:
            name: ims_operations_manager
          vars:
            om: "stop"
```

Example Playbook: Query OM
----------------

```yaml 
        - include_role:
            name: ims_operations_manager
          vars:
            om: "query"
```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


