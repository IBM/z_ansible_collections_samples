# Single module playbooks for the IBM Z HMC Collection

The single module playbooks are very simple playbooks that demonstrate the use
of a single module. Their filenames start with ``module_``.

Any resources created by them have a name that starts with ``sample-``.

These playboos are not described in detail and are assumed to be self-explanatory
together with the
[IBM Z HMC Collection documentation](https://zhmcclient.github.io/zhmc-ansible-modules/modules.html).

Here is an example for invoking the
[module_create_partition.yml](../playbooks/module_create_partition.yml)
playbook that creates a partition named ``sample-part-1``:

```
$ ansible-playbook -i inventory.yml module_create_partition.yml

PLAY [my_hmc_list] *****************************************************************************************************

TASK [Gathering Facts] *************************************************************************************************
ok: [myhmc13]

TASK [Ensure partition sample-part-1 exists and is stopped] ************************************************************
changed: [myhmc13 -> localhost]

TASK [Print the result] ************************************************************************************************
ok: [myhmc13] => {
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
                    "description": "The DPM storage management approach in which FCP and FICON storage resources are defined in Storage Groups, which are attached to Partitions.",
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
            "object-id": "d11a3452-c9b3-11eb-a3ef-00106f239d19",
            "object-uri": "/api/partitions/d11a3452-c9b3-11eb-a3ef-00106f239d19",
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

PLAY RECAP *************************************************************************************************************
myhmc13                    : ok=3    changed=1    unreachable=0    failed=0    skipped=0    rescued=0    ignored=0   
```

And here is an example for invoking the
[module_delete_partition.yml](../playbooks/module_delete_partition.yml)
playbook that deletes that partition again:

```
$ ansible-playbook -i inventory.yml module_delete_partition.yml

PLAY [my_hmc_list] *****************************************************************************************************

TASK [Gathering Facts] *************************************************************************************************
ok: [myhmc13]

TASK [Ensure partition sample-part-1 does not exist] *******************************************************************
changed: [myhmc13 -> localhost]

TASK [Print the result] ************************************************************************************************
ok: [myhmc13] => {
    "result": {
        "changed": true,
        "failed": false,
        "partition": {}
    }
}

PLAY RECAP *************************************************************************************************************
myhmc13                    : ok=3    changed=1    unreachable=0    failed=0    skipped=0    rescued=0    ignored=0   
```
