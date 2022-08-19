Initialize IMS
=========

This role performs multiple tasks:
* Allocate datasets and reserve ports for IMS Connect
* Send PROCLIB member templates to z/OS
* Send templates required for IMS Provisioning to z/OS

Requirements
------------
* IBM z/OS core collection 1.4.0

Role Variables
--------------
| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |                                                                                                                       
| DYNAMICALLY_RESERVE_PORTS               | If it's true, dynamic ports will be reserved for IMS Connects                                                                                                                           |                                                                                                                          |
| DFS_INIT_JAVA_CONF               | If it's true, Java (JMP region) will be enabled                                                                                                                            |
| send_procs               | If it's true, PROCLIB members will be created 
                                                                                          




Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: ims_initialize
      vars:
        send_procs: true
        DFS_INIT_JAVA_CONF: false

```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
