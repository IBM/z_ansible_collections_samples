db2_get_app_id
=========

Get the application ID of an application.

Requirements
------------

None.

Role Variables
--------------

| Variable           | Definition                                                                             |
|--------------------|----------------------------------------------------------------------------------------|
| doe_server    | Server name used to access the DOE REST APIs|
| doe_server_port            | Port number of the Db2 DevOps Experience Server. Default: 12023|
| base_url       |  Base URL used to access the DOE REST APIs|
| username              | Username used to access the DOE REST APIs|
| password              | Password used to access the DOE REST APIs|
| app_name              | Name of the application that the instance is provisioned from|

Dependencies
------------

None.

Example Playbook
----------------

```yaml
    - include_role:
        name: db2_get_app_id
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
