System Definition Services
=========

Allocate the following system definition datasets depending on role variables:
* preprocessing 
* stage 1 and stage 2

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


Role Variables
--------------
| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| preproces                | If it's true, preprocessor utility datasets will be allocated.                                                                                                                             |
| stage1                           | If it's true, sysdef stage 1 datasets will be allocated.       
| stage2                |  If it's true, sysdef stage 1 datasets will be allocated.                                                                                                                            |
| 



Dependencies
------------

None

Example Playbook
----------------

```yaml

       - include_role:
            name: ims_dataset
          vars:
            work_dataset: allocate
            sysdef_datasets: allocate
            runtime_datasets: allocate

```

## Copyright

© Copyright IBM Corporation 2022

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


