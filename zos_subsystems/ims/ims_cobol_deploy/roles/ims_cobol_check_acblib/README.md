ims_cobol_check_acblib
=========

Check which ACB library is inactive.

Requirements
------------

Ansible Collection `ibm.ibm_zos_ims`

Role Variables
--------------

| Variable           | Definition                                                                             |
|--------------------|----------------------------------------------------------------------------------------|
| INACTIVE_ACBLIB    | The name of the inactive ACBLIB, dynamically overridden during app deployment          |
| IMSPLEX            | The name of the IMSplex.                                                              |
| ROUTE              | The SSID of the IMS instance.                                                          |

Dependencies
------------

None.

Example Playbook
----------------

```yaml
    - include_role:
        name: ims_cobol_check_acblib
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


