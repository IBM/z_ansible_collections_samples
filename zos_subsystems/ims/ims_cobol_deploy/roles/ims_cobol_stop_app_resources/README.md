ims_cobol_stop_app_resources
=========

Perform delete for IMS transactions and programs.

Requirements
------------

Ansible Collection `ibm.ibm_zos_ims`

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ROUTE                      | The SSID of the IMS instance used by the application                                                                                                                        |

Dependencies
------------

None


Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_stop_app_resources
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
