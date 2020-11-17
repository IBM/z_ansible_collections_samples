Prepare User Exit Services
=========

Prepares a user to exit IMS security and connections.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **prepare_exit**

  When prepare_exit = true, prepares to exit IMS security and connections.


Dependencies
------------

None

Example Playbook:
----------------

```yaml

         - include_role:
              name: ims_exit
            vars:
              prepare_exit: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


