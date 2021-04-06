Save templates to dataset
=========

This role is a utility used by other roles to save template files into a dataset on z/OS system.

Requirements
------------

* None

Role Variables
--------------

- ### ** datasets **
   The dataset name where the template files will be saved.


Dependencies
------------

None

Example Playbook
----------------

```yaml
  - include_role: 
      name: save-templates-to-datasets
        datasets: <dataset-name>

```

## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)
