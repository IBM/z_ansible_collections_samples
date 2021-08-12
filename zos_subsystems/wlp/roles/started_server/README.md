Role Name
=========

Define STARTED profiles for the PROCs for the server process. 

Requirements
------------

Ansible Collection: ibm.ibm_zos_core


Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| PROC_LIB      | The name of procedure library | `host_vars/zos_host.yml`                         |
| SERVER_PROC   | The unique name for the STARTED command to run the server | `host_vars/zos_host.yml`        |
| ANGEL_PROC    | The unique name for the STARTED command to run the angel process | `host_vars/zos_host.yml` | 
| TARGET_USER   | USERID for the angel process | `host_vars/zos_host.yml` | 
| USER_GROUP    | Group for TARGET_USER | `host_vars/zos_host.yml` | 

Dependencies
------------

None

Example Playbook
----------------

```yaml
    - include_role:
        name: started_server
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under Apache License, Version 2.0

Author Information
------------------

Stephanie Lieu - @stephanie-lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
