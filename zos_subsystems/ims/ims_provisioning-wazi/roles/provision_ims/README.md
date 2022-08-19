Role Name
=========

This role provisions an IMS instance.  It allocates, copies all required datasets and starts 
up IMS services.

Requirements
------------
* IBM z/OS core collection 1.4.0
* IBM z/OS IMS collection 1.2.0


Role Variables
--------------

* ansible_user: This user ID (used to log in the USS) is passed to this role.


Dependencies
------------

This roles utilizes many other roles in IMS to accomplish the provisioning task.

Example Playbook
----------------

This role can be included in another role as the following:

    - include_role:
        name: provision_ims

## Copyright
© Copyright IBM Corporation 2022

## License
Licensed under
[Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0).
