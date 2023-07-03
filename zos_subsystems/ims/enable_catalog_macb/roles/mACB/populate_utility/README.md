populate_utility
=========

Run the [IMS Catalog Populate utility (DFS3PU00)](https://www.ibm.com/docs/en/ims/15.3.0?topic=utilities-ims-catalog-populate-utility-dfs3pu00#ims_catalog_pop_utility) in update mode by using the `DFSCP001` resource. 

> **_Note:_** Because the DFS3PU00 utility runs in update mode, you're required to create an image copy before running the DFS3PU00 utility to keep the integrity of the IMS catalog.


Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.4.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.2.0](https://galaxy.ansible.com/ibm/ibm_zos_ims)

Role Variables
--------------

None

Dependencies
------------

Image copies of the loaded IMS catalog data sets. 

Copyright
---------

© Copyright IBM Corporation 2023

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @steph-lieu

Support
-------

Refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
