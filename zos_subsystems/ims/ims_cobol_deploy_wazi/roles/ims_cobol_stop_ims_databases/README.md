ims_cobol_stop_ims_databases
=========

Perform delete operation for IMS databases

Requirements
------------

Ansible Collection `ibm.ibm_zos_ims`

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ROUTE                      | The SSID of the IMS instance used by the application                                                                                                                       |

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_stop_ims_databases
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
