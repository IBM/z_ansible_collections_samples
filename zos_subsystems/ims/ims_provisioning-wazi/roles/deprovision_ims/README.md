Role Name
=========

This role de-provisions an IMS instance.  It stops all IMS services and delete all datasets created by provision_ims role.

Requirements
------------
* IBM z/OS core collection 1.4.0-beta.1
* IBM z/OS IMS collection 1.2.0-beta.1


Role Variables
--------------

* ansible_user: This user ID (used to log in the USS) is passed to this role.


Dependencies
------------

This roles utilizes many other roles in IMS to accomplish the deprovision task.

Example Playbook
----------------

This role can be included in another role as the following:

    - include_role:
        name: deprovision_ims

## License
Licensed under [Apache License](https://opensource.org/licenses/Apache-2.0).


Author Information
------------------

An optional section for the role authors to include contact information, or a website (HTML is not allowed).
