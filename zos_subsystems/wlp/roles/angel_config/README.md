angel_config
=========

Define STARTED profiles for an angel process and enable the server to connect to the angel process.

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.3.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| PROC_LIB      | Name of the procedure library | `host_vars/zos_host.yml`                         |
| SERVER_PROC   | Unique name for the STARTED command to run the server process | `host_vars/zos_host.yml`        |
| ANGEL_PROC    | Unique name for the STARTED command to run the angel process | `host_vars/zos_host.yml` |
| TARGET_USER   | User id of the angel process | `host_vars/zos_host.yml` |
| USER_GROUP    | Group of the target user | `host_vars/zos_host.yml` |


Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: angel_config
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @stephanie-lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
