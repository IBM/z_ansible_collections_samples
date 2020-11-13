IMS Structured Call Interface (SCI)
=========

This role creates, starts, stops and queries IMS Structured Call Interface (SCI).

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **sci**

  Specifies what action to take:
  - "config": create configuration files for SCI
  - "start":  start RM
  - "stop":   stop RM
  - "query":   query RM status


Dependencies
------------

None

Example Playbook: Create configuration files for SCI
----------------

```yaml 
        - include_role:
            name: ims_structured_call_interface
          vars:
            om: "config"
```

Example Playbook: Start RM
----------------

```yaml 
        - include_role:
            name: ims_structured_call_interface
          vars:
            om: "start"
```

Example Playbook: Stop RM
----------------

```yaml 
        - include_role:
            name: ims_structured_call_interface
          vars:
            om: "stop"
```

Example Playbook: Query RM
----------------

```yaml 
        - include_role:
            name: ims_structured_call_interface
          vars:
            om: "query"
```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


