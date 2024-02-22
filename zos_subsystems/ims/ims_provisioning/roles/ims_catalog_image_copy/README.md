image_copy
=========

Create [image copies](https://www.ibm.com/docs/en/ims/15.3.0?topic=copies-image-ims-image-copy-utilities) by using the [Database Image Copy utility (DFSUDMP0)](https://www.ibm.com/docs/en/ims/15.3.0?topic=utilities-database-image-copy-utility-dfsudmp0) of the loaded IMS catalog data sets. You need image copies when catalog is DBRC enabled.

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.5.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.3.0-beta.1 or later](https://galaxy.ansible.com/ibm/ibm_zos_ims)

Role Variables
--------------

- `to_image_copy` from [host_vars/zos_host.yml](host_vars/zvm.yml) - List of dictionaries for each image copy data set
- 'image': 
    - 'copy': take the image copies
    - 'delete': delete the image copies

Example of one dictionary entry:   
  ```yaml
  { dataset: DFSCD000, dbd: DFSCD01, dsn: IMSTESTL.IMS1.DFSCD000.A00001, ddn: DFSCD01A, icdsn: IMSTESTL.DFSCD01.DFSCD01A.IC.CD01A }
  ```

Dependencies
------------

None

Example Playbook
----------------
```yaml
    - include_role:
        name: roles/image_copy
      with_items: 
        - "{{ to_image_copy }}" 
      
```
Copyright
---------

© Copyright IBM Corporation 2024

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @steph-lieu

Support
-------

Refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
