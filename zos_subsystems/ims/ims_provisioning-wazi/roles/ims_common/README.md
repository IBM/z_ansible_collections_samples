Common Services
=========

Provide the following services to IMS:
* Start and stop IMS Control region and IMS Connect
* Cold start IMS
* Wait for IMS ready


Requirements
------------

* Ansible Collection: ibm.ibm_zos_core and ibm.ibm_zos_ims

Role Variables
--------------

| Variable                           | Definition                                                                                                                                                          |
| ---------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DFS_IMS_SSID                | The ID of the IMS                                                                                                                           |
| DFS_IMSPlex                           | The ID of the IMS Plex       
| icon                | If icon = "start", IMS Connect will be started.  If icon = "stop", it will be stopped                                                                                                                           |
| ctl                | If ctl = "start", IMS Control region will be started.  If ctl = "stop", it will be stopped                                                                                                                           |
| cold_start                | If icon = true, IMS will be cold-started                                                                                                                            |
| wait_for_job                | If wait_for_job = true, Common Services will wait for IMS to become ready   

                                                                                          

Dependencies
------------

None

Example Playbook
----------------

```yaml
        - include_role:
            name: ims_common
            tasks_from: ims_common
          vars:
            icon: start
            ctl: start
            wait_for_job: false
            cold_start: true

```


## Copyright

© Copyright IBM Corporation 2020

License
-------

Copyright (c) IBM Corporation 2020 Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

Author Information
------------------

An Lam (an.lam@ibm.com)
