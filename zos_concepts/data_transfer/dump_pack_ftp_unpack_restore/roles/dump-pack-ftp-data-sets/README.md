dump-pack-ftp-data-sets
=========

Dump, pack and transfer a data set to another system using FTP.

Requirements
------------

- Ansible Collection `ibm.ibm_zos_core`
- Role `get-estimated-size-of-data-sets`

Role Variables
--------------

| Variable Name     | Description                                                                                                 |
| ----------------- | ----------------------------------------------------------------------------------------------------------- |
| data_sets_to_dump | a list of data sets contained in an archive you wish to include in dump                                     |
| archive_data_set  | the name to use for the data set archive if not provided, module-generated name will be used                |
| terse_data_set    | the name to use for the tersed archive data set if not provided, module-generated name will be used         |
| target_data_set   | the name to give to the tersed archive after FTP transfer if not provided terse_data_set value will be used |
| input_volume      | the volume containing the data sets to archive if input volume not provided system defaults will be used    |
| target_hostname   | the host name of the target host which will receive the tersed data set                                     |
| target_user       | the user name to use for connecting to the target host                                                      |
| target_password   | the password to use for connecting to the target host                                                       |
| delete: yes       | should terse and archive data sets be deleted upon successful transfer?                                     |

Example Playbook
----------------

```yaml
- hosts: source_system
  collections:
    - ibm.ibm_zos_core
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Dump, pack, and transfer data sets
      include_role:
        name: dump-pack-ftp-data-sets
        public: yes
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
