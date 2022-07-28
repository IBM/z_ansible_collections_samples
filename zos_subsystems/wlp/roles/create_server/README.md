create_server
=========

Create an instance of a WebSphere Liberty server.

Requirements
------------

None

Role Variables
--------------

| Variable      | Definition                             | Set                                              |
| ------------- | ---------------------------------------|--------------------------------------------------|
| server_instance_name | Unique name of the instance of the Liberty server | `host_vars/zos_host.yml`       |
| liberty_bin_path | Path to the Liberty installation bin folder | `host_vars/zos_host.yml`            |

Dependencies
------------

None

Example Playbook
----------------
```yaml
  - include_role:
          name: create_server
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @Stephanie-Lieu or @steph-lieu

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
