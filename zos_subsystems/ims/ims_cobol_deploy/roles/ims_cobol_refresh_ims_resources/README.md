ims_cobol_refresh_ims_resources
=========

Stop and restart the IMS transaction, program, and MPP region pertaining to the application.

Requirements
------------

Ansible Collection `ibm.ibm_zos_ims`


Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ROUTE                      | The SSID of the IMS instance                                                                                                              |

Dependencies
------------

Roles:

* ims_cobol_copy_extract_files
* ims_cobol_create_app_datasets

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_refresh_ims_resources
```

Copyright
---------

© Copyright IBM Corporation 2021

License
-------

Licensed under Apache License, Version 2.0

Author Information
------------------

Ajay Jashnani - @Ajay-Jashnani

Support
-------

Please refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
