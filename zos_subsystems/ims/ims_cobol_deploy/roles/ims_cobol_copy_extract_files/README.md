ims_cobol_copy_extract_files
=========

Copy and extract COBOL and LOAD program files to USS.

Requirements
------------

None

Role Variables
--------------

| Variable                   | Definition                                                                                                                                                          |
|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| PROJECT_NAME               | Project name to define the IMS COBOL Application                                                                                                        |
| SAMPLE_FILE_DIR            | Location to place the IMS COBOL application files                                                                                                               |
| USE_GIT                    | Variable to indicate if Git should be used to retrieve the latest IMS COBOL configuration                                                                                                       |

Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role:
      name: ims_cobol_copy_extract_files
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
