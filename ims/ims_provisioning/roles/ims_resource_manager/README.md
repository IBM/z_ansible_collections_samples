IMS Resource Manager
=========

This role creates, starts, stops and queries IMS Resource Manager.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **rm**

  Specifies what action to take:
  - "config": create configuration files for Resource Manager (RM)
  - "start":  start RM
  - "stop":   stop RM
  - "query":   query RM status


Dependencies
------------

None

Example Playbook: Create configuration files for RM
----------------

```yaml 
        - include_role:
            name: ims_resource_manager
          vars:
            om: "config"
```

Example Playbook: Start RM
----------------

```yaml 
        - include_role:
            name: ims_resource_manager
          vars:
            om: "start"
```

Example Playbook: Stop RM
----------------

```yaml 
        - include_role:
            name: ims_resource_manager
          vars:
            om: "stop"
```

Example Playbook: Query RM
----------------

```yaml 
        - include_role:
            name: ims_resource_manager
          vars:
            om: "query"
```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


