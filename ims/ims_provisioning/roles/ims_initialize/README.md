Initialize IMS
=========

This role sends provisioning files to zOS and dynamically reserves IMS Connection ports.

Requirements
------------

* Ansible Collection: `ibm.ibm_zos_core` and `ibm.ibm_zos_ims`


Role Variables
--------------

- ### **DYNAMICALLY_RESERVE_PORTS**

  If true, some ports are dynamically reserved for IMS Connection.

- ### **provision**

  If true, send template files (required for provisioning) to zOS.

- ### **DFS_INIT_JAVA_CONF**

  If true, send config Java files to zOS.

- ### **send_procs**

  If true, send PROCLIB member templates to zOS.



Dependencies
------------

None

Example Playbook: How to send provisioning files to zOS.
----------------

```yaml 
        - include_role:
            name: ims_initialize
          vars:
            DYNAMICALLY_RESERVE_PORTS: false
            send_procs: true
            DFS_INIT_JAVA_CONF: false
            provision: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


