confirm
=========

Confirm that the IMS catalog is provisioned properly by using the [DL/I test program (DFSDDLT0)](https://www.ibm.com/docs/en/ims/15.3.0?topic=reference-dli-test-program-dfsddlt0). 

The role uses the [STATUS statement](https://www.ibm.com/docs/en/ims/15.3.0?topic=reference-status-statement) to print the `COMMENTS, CALL, COMPARE, PCB, and SEGMENT DATA` of the newly provisioned IMS catalog. 

The role includes a `fail` statement that checks for the success return code `0`. If the sucess return code `0` is not returned, it returns an error message. 

Requirements
------------

[IBM&reg; z/OS&reg; core collection 1.4.0 or later](https://galaxy.ansible.com/ibm/ibm_zos_core)

[IBM&reg; z/OS&reg; IMS collection 1.2.0](https://galaxy.ansible.com/ibm/ibm_zos_ims)

Role Variables
--------------

None

Dependencies
------------

None


Copyright
---------

© Copyright IBM Corporation 2023

License
-------

Licensed under [Apache License, Version 2.0](https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

Stephanie Lieu - @Stephanie-Lieu or @steph-lieu

Support
-------

Refer to the [support section](https://github.com/IBM/z_ansible_collections_samples/blob/master/README.md#support) for more details.
