get-estimated-size-of-data-sets
=========

Get the estimated storage size (in cylinders) used for one or more data sets. The estimated size is rounded up to the nearest cylinder

Sets a fact named `size_in_cyls` with the keys `primary` and `secondary`, representing the estimated primary and secondary space in cylinders.

Estimated secondary space is always equal to estimated primary space.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`


Role Variables
--------------

| Variable Name | Description                                                                                               |
| ------------- | --------------------------------------------------------------------------------------------------------- |
| data_sets     | a list of data sets to determine used storage for. Needs to be a list even if only one data set provided. |

Example Playbook
----------------

```yaml
- hosts: destination_system
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Estimate the total used size of all provided data sets
      include_role:
        name: get-estimated-size-of-data-sets
      vars:
        data_sets:
          - user.private.proclib
          - user.test.ds1

    - debug:
        msg: "Estimated primary space is is {{ size_in_cyls.get('primary') }}"

    - debug:
        msg: "Estimated secondary space is {{ size_in_cyls.get('secondary') }}"
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
