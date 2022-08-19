ims_racf
=========

Add IMS modules into Security Authorization Control.

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0 


Role Variables
--------------

None.


Dependencies
------------

None

Example Playbook
----------------

```yaml

    - include_role:
        name: ims_racf
      vars:
        prep_racf: true

```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
