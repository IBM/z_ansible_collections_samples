ims_apf
=========

Add IMS datasets to the authorized program list.

Requirements
------------
* IBM z/OS core collection 1.4.0-beta.1
* IBM z/OS IMS collection 1.2.0-beta.1



Role Variables
--------------

* ansible_user: This user ID (used to log in the USS) is passed to this role.


Dependencies
------------

None

Example Playbook
----------------

```yaml
    - include_role:
        name: ims_apf
      vars:
        auth_datasets: true

```

## Copyright

© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
