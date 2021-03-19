zcee_generate_sar
=========

This role generates the API Archive file when provided with the project directory and archive file name.

Requirements
------------

Ensure that the z/OS® Connect EE build toolkit is installed.

Role Variables
--------------

* `project_directory`: file path to the root directory of user's project.

* `archive_file_name`: name of the new archive file to be generated.


Example Playbook
----------------

```yaml
- hosts: all
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Generate Service Archive
      include_role:
        name: zcee_generate_sar
      vars:
        project_directory: path/to/root/project/directory
        archive_file_name: archives/api_archive.aar
```

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

- Omar Elbarmawi omar@ibm.com, [@oelbarmawi](https://github.com/oelbarmawi)

Copyright
---------

© Copyright IBM Corporation 2020
