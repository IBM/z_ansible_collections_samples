IMS Operations Manager
=========

This role creates, starts, stops and queries IMS Operations Manager.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **om**

  Specifies what action to take:
  - "config": create configuration files for OM
  - "start":  start OM
  - "stop":   stop OM
  - "query"   query OM status


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


