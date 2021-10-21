db2_put_infra_discovery
=========

Initiate infrastructure discovery.

Requirements
------------

None.

Role Variables
--------------

| Variable           | Definition                                                                             |
|--------------------|----------------------------------------------------------------------------------------|
| doe_server    | Server name used to access the DOE REST APIs          |
| doe_server_port            | Port number of the Db2 DevOps Experience Server. Default: 12024                                                              |
| base_url       |  Base URL used to access the DOE REST APIs|
| username              | Username used to access the DOE REST APIs                                                        |
| password              | Password used to access the DOE REST APIs|

Dependencies
------------

None.

Example Playbook
----------------

```yaml
    - include_role:
        name: db2_put_infra_discovery
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under Apache License, Version 2.0

Author Information
------------------

Dipti Gandhi - @ddgandhi

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
