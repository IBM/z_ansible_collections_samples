.. Copyright 2020 IBM Corp. All Rights Reserved.
..
.. Licensed under the Apache License, Version 2.0 (the "License");
.. you may not use this file except in compliance with the License.
.. You may obtain a copy of the License at
..
..    http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS,
.. WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
.. See the License for the specific language governing permissions and
.. limitations under the License.
..

Sample module playbooks
-----------------------

The sample module playbooks are very simple playbooks that demonstrate the use
of a single module. Their filenames start with ``module_``.

Any resources created by them have a name that starts with ``sample-``.

These playboos are not described in detail and are assumed to be self-explanatory
together with the
`zhmc module documentation <https://zhmcclient.github.io/zhmc-ansible-modules/modules.html>`_.

Here is an examle for invoking the sample module playbooks for creating and
deleting a partition:

.. code-block:: bash

    $ ansible-playbook module_create_partition.yml

    PLAY [localhost] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    TASK [Gathering Facts] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    ok: [localhost]

    TASK [Ensure partition sample-part-1 exists and is stopped] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    changed: [localhost]

    TASK [Print the result] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    ok: [localhost] => {
        "result": {
            "changed": true,
            "failed": false,
            "partition": {
                "acceptable-status": [
                    "active"
                ],
                "access-basic-counter-set": false,
                "access-basic-sampling": false,
                "access-coprocessor-group-set": false,
                "access-crypto-activity-counter-set": false,
                "access-diagnostic-sampling": false,
                "access-extended-counter-set": false,
                "access-global-performance-data": false,
                "access-problem-state-counter-set": false,
                "auto-start": false,
                "autogenerate-partition-id": true,
                "available-features-list": [
                    {
                        "description": "The DPM storage management approach in
                         which FCP and FICON storage resources are defined in
                         Storage Groups, which are attached to Partitions.",
                        "name": "dpm-storage-management",
                        "state": true
                    }
                ],
                "boot-configuration-selector": 0,
                "boot-device": "none",
                "boot-ftp-host": null,
                "boot-ftp-insfile": null,
                "boot-ftp-username": null,
                "boot-iso-image-name": null,
                "boot-iso-ins-file": null,
                "boot-logical-unit-number": "",
                "boot-network-device": null,
                "boot-os-specific-parameters": "",
                "boot-record-lba": "0",
                "boot-removable-media": null,
                "boot-removable-media-type": null,
                "boot-storage-device": null,
                "boot-storage-volume": null,
                "boot-timeout": 60,
                "boot-world-wide-port-name": "",
                "class": "partition",
                "cp-absolute-processor-capping": false,
                "cp-absolute-processor-capping-value": 1.0,
                "cp-processing-weight-capped": false,
                "cp-processors": 0,
                "crypto-configuration": null,
                "current-cp-processing-weight": 1,
                "current-ifl-processing-weight": 1,
                "degraded-adapters": [],
                "description": "This is partition sample-part-1",
                "has-unacceptable-status": true,
                "hba-uris": [],
                "hbas": [],
                "ifl-absolute-processor-capping": false,
                "ifl-absolute-processor-capping-value": 1.0,
                "ifl-processing-weight-capped": false,
                "ifl-processors": 2,
                "initial-cp-processing-weight": 100,
                "initial-ifl-processing-weight": 200,
                "initial-memory": 1024,
                "ipl-load-parameter": "",
                "is-locked": false,
                "maximum-cp-processing-weight": 999,
                "maximum-ifl-processing-weight": 800,
                "maximum-memory": 1024,
                "minimum-cp-processing-weight": 1,
                "minimum-ifl-processing-weight": 50,
                "name": "sample-part-1",
                "nic-uris": [],
                "nics": [],
                "object-id": "dff1eccc-370f-11eb-a525-00106f239d19",
                "object-uri": "/api/partitions/dff1eccc-370f-11eb-a525-00106f239d19",
                "os-name": "",
                "os-type": "",
                "os-version": "",
                "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                "partition-id": null,
                "permit-aes-key-import-functions": true,
                "permit-cross-partition-commands": false,
                "permit-des-key-import-functions": true,
                "processor-management-enabled": false,
                "processor-mode": "shared",
                "reserve-resources": false,
                "reserved-memory": 0,
                "short-name": "SAMPLEPA",
                "status": "stopped",
                "storage-group-uris": [],
                "threads-per-processor": 1,
                "type": "linux",
                "virtual-function-uris": [],
                "virtual-functions": []
            }
        }
    }

    PLAY RECAP \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    localhost                  : ok=3    changed=1    unreachable=0    failed=0
                                 skipped=0    rescued=0    ignored=0

    $ ansible-playbook sample_delete_partition.yml

    PLAY [localhost] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*

    TASK [Gathering Facts] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    ok: [localhost]

    TASK [Ensure partition sample-part-1 does not exist] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    changed: [localhost]

    TASK [Print the result] \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    ok: [localhost] => {
        "result": {
            "changed": true,
            "failed": false,
            "partition": {}
        }
    }

    PLAY RECAP \*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*
    localhost                  : ok=3    changed=1    unreachable=0    failed=0
                                 skipped=0    rescued=0    ignored=0
