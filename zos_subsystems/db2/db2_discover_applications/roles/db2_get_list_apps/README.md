db2_get_list_apps
=========

Get the list of applications for the specific user.

Requirements
------------

None.

Role Variables
--------------

| Variable           | Definition                                                                             |
|--------------------|----------------------------------------------------------------------------------------|
| doe_server    | Server name used to access the Db2 DevOps Experience(DOE) REST APIs.          |
| doe_server_port            | Port number of the DOE Server. Default: 12023.                                                              |
| base_url       |  Base URL used to access the DOE REST API.|
| username              | Username used to access the DOE REST API.                                                        |
| password              | Password used to access the DOE REST API.|


Dependencies
------------

None.

Example Playbook
----------------

```yaml
    - include_role:
        name: db2_get_list_apps
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
