ims_cobol_define_mpp_region
=========

Create and start an IMS MPP region.

Requirements
------------

None

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|                                                                                                                          |
| TSO_USERID                                         | TSO USERID for executing the environment setup                      |
|                                                                                                                          |
| ZOS_CSSLIB                                         | Specifies the name of the CSSLIB data set                          |                                                                                                |                                                                                                                          |
| ZOS_LERUNLIB                                       | Data set name used as Language Environment runtime library          |                                                                                                    |                                                                                                                          |
| ZOS_MACLIB                                         | Data set containing the zOS macro library                                |
| DFS_appname                                        | The IMS application program name that will be used during the create program step of the playbook. This name will also be used as the IMS PSB Name.
|                                                                                                                          |
| DFS_traname                                        | The transaction name used for the application                  |                                                                                                                 |                                                                                                                          |
| DFS_tranclass                                      | Transaction class used by the application                      |                                                                                                 |
| DFS_tranclass1                                     | The first transaction class used by the application. This class is likely the same as DFS_tranclass.                                                                                               |
| DFS_tranclass2                                     | The second transaction class used by the application          |                                                                                                |
| DFS_tranclass3                                     | The third transaction class used by the application           |                                                                                                |
| DFS_tranclass4                                     | The fourth transaction class used by the application          |                                                                 |

Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_define_mpp_region
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
