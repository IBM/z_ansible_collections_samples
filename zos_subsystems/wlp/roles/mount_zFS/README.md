mount_zFS
=========

Mount the created z/OS file system (zFS) to the defined user directory.

Requirements
------------

create_zFS role. A data set must be created and formatted before being mounted. 

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| data_set_name | The name of the data set to be mounted | `host_vars/zos_host.yml`                         |
| environment_vars.WLP_USER_DIR | The path to the mount point | `host_vars/zos_host.yml`                    |

Dependencies
------------

None 

Example Playbook
----------------

```yaml
    - include_role:
        name: mount_zFS
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
