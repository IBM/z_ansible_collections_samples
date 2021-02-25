zcee_generate_ara
=========

This role generates the API Requester Archive file when provided with the properties and archive file names.

Requirements
------------

Ensure that the z/OS® Connect EE build toolkit is installed.

Role Variables
--------------

* `properties_file`: file path to the properties file used to generate the API requester archive.

* `archive_file_name`: name of the new archive file to be generated.


Example Playbook
----------------

```yaml
- hosts: all
  gather_facts: no
  environment: "{{ environment_vars }}"

  tasks:
    - name: Generate API Requester Archive
      include_role:
        name: zcee_generate_ara
      vars:
        project_directory: path/to/properties/file
        archive_file_name: archives/api_archive.ara
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
