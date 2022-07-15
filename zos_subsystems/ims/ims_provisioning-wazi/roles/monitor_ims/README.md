monitor_ims
=========

This role queries and starts many IMS Services (OM, SCI, RM) if they are not up.

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

This role can be included in another role as the following:

    - include_role:
        name: monitor_ims


## Copyright
© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).


