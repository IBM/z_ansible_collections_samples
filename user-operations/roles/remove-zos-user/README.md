remove-zos-user
=========

Remove a user from a z/OS system.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`

Role Variables
--------------

- ### **userid**

  Specifies the user to be removed from RACF.

- ### **new_owner**

  Specifies a RACF-defined user or group that owns the group data set profiles now owned by the
  user to be removed.

  If you omit this operand when group data set profiles exist that require a new owner, RACF does not
  remove the user from the group. (Group data set profiles are data set profiles whose names are qualified
  by the group name or begin with the value supplied by an installation exit.)

  The new owner of the group data set profiles must have at least USE authority in the specified group.

  Do not specify a user who is being removed from the group as the new data set profile owner.

- ### **user_catalog**

  Specifies the user catalog entryname for which an alias is defined.

- ### **omvs_home_directory**

  Specifies the user's z/OS UNIX initial directory path name.

  This is the current working directory for the user's process when the user enters the TSO/E.

  When you define a HOME directory name to RACF, it can contain 1 - 1023 characters.

  The HOME path name can consist of any characters.
  If left blank, home directory is /u/userid.

- ### **omvs_zfs_data_set_name**

  Specifies the name of the data set to be used when removing ZFS data set.

- ### **delete_user_data_sets**

  Specifies if the user data sets should be deleted.

  This job attempts to delete any data sets with the HLQ matching the user ID of the user to be removed.

Example Playbook
----------------

```yaml
- hosts: all
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Remove a user from z/OS system
      include_role:
        name: remove-zos-user
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Blake Becker blake.becker@ibm.com, [@blakeinate](https://github.com/blakeinate)

Copyright
---------

© Copyright IBM Corporation 2020
