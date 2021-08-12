configure_server
=========

Transfer custom configuration files, such as  `bootstrap.properties`, `jvm.options`, or `server.xml`, for the server based on which files are in the `/templates` folder.  
If the file is in the `/templates` folder, it will transfer the file, otherwise it will continue going through the playbook.

Requirements
------------

None 

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| liberty_server_path | The path to the current Liberty server instance | `host_vars/zos_host.yml`          |

Dependencies
------------

None 

Example Playbook
----------------
```yaml
  - include_role:
          name: configure_server
```
Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under Apache License, Version 2.0

Author Information
------------------

Stephanie Lieu - @Stephanie-Lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
