unpack-restore-data-sets
=========

Unpack and restore data sets from a tersed data set dump.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`
- Role `get-estimated-size-of-data-sets`

Role Variables
--------------

| Variable Name        | Description                                                                                                                      |
| -------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| data_sets_to_restore | a list of data sets contained in an archive you wish to restore                                                                  |
| archive_data_set     | the name to use for the data set archive. if not provided, module-generated name will be used                                    |
| terse_data_set       | the name to use for the tersed archive data set                                                                                  |
| output_volume        | the volume where data sets restored from an archive should be placed. if output volume not provided system defaults will be used |
| new_hlq              | the HLQ to give to the data sets after restore. if one is not provided, original will be used                                    |
| delete               | should terse and archive data sets be deleted upon successful restore?                                                           |
| replace              | should existing data sets be overwritten during restore?                                                                         |

Example Playbook
----------------

```yaml
- hosts: destination_system
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Unpack and restore data sets
      include_role:
        name: unpack-restore-data-sets
```

License
-------

Copyright (c) IBM Corporation 2020
Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


Author Information
------------------

- Blake Becker blake.becker@ibm.com, [@blakeinate](https://github.com/blakeinate)

# Copyright

© Copyright IBM Corporation 2020
