
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_partition.py

.. _zhmc_partition_module:


zhmc_partition -- Create partitions
===================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Gather facts about a partition of a CPC (Z system), including its HBAs, NICs, and virtual functions.
- Create, update, or delete a partition. The HBAs, NICs, and virtual functions of the partition are managed by separate Ansible modules.
- Start or stop a partition.


Requirements
------------

- Access to the WS API of the HMC of the targeted Z system (see :term:`HMC API`).
- The targeted Z system must be in the Dynamic Partition Manager (DPM) operational mode.




Parameters
----------


hmc_host
  The hostname or IP address of the HMC.

  | **required**: True
  | **type**: str


hmc_auth
  The authentication credentials for the HMC.

  | **required**: True
  | **type**: dict


  userid
    The userid (username) for authenticating with the HMC.

    | **required**: True
    | **type**: str


  password
    The password for authenticating with the HMC.

    | **required**: True
    | **type**: str


  ca_certs
    Path name of certificate file or certificate directory to be used for verifying the HMC certificate. If null (default), the path name in the 'REQUESTS_CA_BUNDLE' environment variable or the path name in the 'CURL_CA_BUNDLE' environment variable is used, or if neither of these variables is set, the certificates in the Mozilla CA Certificate List provided by the 'certifi' Python package are used for verifying the HMC certificate.

    | **required**: False
    | **type**: str


  verify
    If True (default), verify the HMC certificate as specified in the ``ca_certs`` parameter. If False, ignore what is specified in the ``ca_certs`` parameter and do not verify the HMC certificate.

    | **required**: False
    | **type**: bool
    | **default**: True



cpc_name
  The name of the CPC with the target partition.

  | **required**: True
  | **type**: str


name
  The name of the target partition.

  | **required**: True
  | **type**: str


state
  The desired state for the partition. All states are fully idempotent within the limits of the properties that can be changed:

  * ``absent``: Ensures that the partition does not exist in the specified CPC.

  * ``stopped``: Ensures that the partition exists in the specified CPC, has the specified properties, and is in one of the inactive statuses ('stopped', 'terminated', 'paused', 'reservation-error').

  * ``active``: Ensures that the partition exists in the specified CPC, has the specified properties, and is in one of the active statuses ('active', 'degraded').

  * ``facts``: Returns the partition properties and the properties of its child resources (HBAs, NICs, and virtual functions).

  | **required**: True
  | **type**: str
  | **choices**: absent, stopped, active, facts


properties
  Dictionary with input properties for the partition, for ``state=stopped`` and ``state=active``. Key is the property name with underscores instead of hyphens, and value is the property value in YAML syntax. Integer properties may also be provided as decimal strings. Will be ignored for ``state=absent``.

  The possible input properties in this dictionary are the properties defined as writeable in the data model for Partition resources (where the property names contain underscores instead of hyphens), with the following exceptions:

  * ``name``: Cannot be specified because the name has already been specified in the ``name`` module parameter.

  * ``type``: Cannot be changed once the partition exists, because updating it is not supported.

  * ``boot_storage_device``: Cannot be specified because this information is specified using the artificial property ``boot_storage_hba_name``.

  * ``boot_network_device``: Cannot be specified because this information is specified using the artificial property ``boot_network_nic_name``.

  * ``boot_storage_hba_name``: The name of the HBA whose URI is used to construct ``boot_storage_device``. Specifying it requires that the partition exists.

  * ``boot_network_nic_name``: The name of the NIC whose URI is used to construct ``boot_network_device``. Specifying it requires that the partition exists.

  * ``crypto_configuration``: The crypto configuration for the partition, in the format of the ``crypto-configuration`` property of the partition (see :term:`HMC API` for details), with the exception that adapters are specified with their names in field ``crypto_adapter_names`` instead of their URIs in field ``crypto_adapter_uris``. If the ``crypto_adapter_names`` field is null, all crypto adapters of the CPC will be used.

  Properties omitted in this dictionary will remain unchanged when the partition already exists, and will get the default value defined in the data model for partitions in the :term:`HMC API` when the partition is being created.

  | **required**: False
  | **type**: dict


expand_storage_groups
  Boolean that controls whether the returned partition contains an additional artificial property 'storage-groups' that is the list of storage groups attached to the partition, with properties as described for the zhmc_storage_group module with expand=true.

  | **required**: False
  | **type**: bool


expand_crypto_adapters
  Boolean that controls whether the returned partition contains an additional artificial property 'crypto-adapters' in its 'crypto-configuration' property that is the list of crypto adapters attached to the partition, with properties as described for the zhmc_adapter module.

  | **required**: False
  | **type**: bool


log_file
  File path of a log file to which the logic flow of this module as well as interactions with the HMC are logged. If null, logging will be propagated to the Python root logger.

  | **required**: False
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   ---
   # Note: The following examples assume that some variables named 'my_*' are set.

   # Because configuring LUN masking in the SAN requires the host WWPN, and the
   # host WWPN is automatically assigned and will be known only after an HBA has
   # been added to the partition, the partition needs to be created in stopped
   # state. Also, because the HBA has not yet been created, the boot
   # configuration cannot be done yet:
   - name: Ensure the partition exists and is stopped
     zhmc_partition:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_partition_name }}"
       state: stopped
       properties:
         description: "zhmc Ansible modules: Example partition 1"
         ifl_processors: 2
         initial_memory: 1024
         maximum_memory: 1024
     register: part1

   # After an HBA has been added (see Ansible module zhmc_hba), and LUN masking
   # has been configured in the SAN, and a bootable image is available at the
   # configured LUN and target WWPN, the partition can be configured for boot
   # from the FCP LUN and can be started:
   - name: Configure boot device and start the partition
     zhmc_partition:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_partition_name }}"
       state: active
       properties:
         boot_device: storage-adapter
         boot_storage_device_hba_name: hba1
         boot_logical_unit_number: 00000000001
         boot_world_wide_port_name: abcdefabcdef
     register: part1

   - name: Ensure the partition does not exist
     zhmc_partition:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_partition_name }}"
       state: absent

   - name: Define crypto configuration
     zhmc_partition:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_partition_name }}"
       state: active
       properties:
         crypto_configuration:
           crypto_adapter_names:
             - adapter1
             - adapter2
           crypto_domain_configurations:
             - domain_index: 0
               access_mode: control-usage
             - domain_index: 1
               access_mode: control
     register: part1

   - name: Gather facts about a partition
     zhmc_partition:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_partition_name }}"
       state: facts
       expand_storage_groups: true
       expand_crypto_adapters: true
     register: part1







See Also
--------

.. seealso::

   - :ref:`zhmc_hba_module`
   - :ref:`zhmc_nic_module`
   - :ref:`zhmc_virtual_function_module`




Return Values
-------------


changed
  Indicates if any change has been made by the module. For ``state=facts``, always will be false.

  | **returned**: always
  | **type**: bool

msg
  An error message that describes the failure.

  | **returned**: failure
  | **type**: str

partition
  For ``state=absent``, an empty dictionary.

  For ``state=stopped|active|facts``, the resource properties of the partition after any changes, including its child resources as described below.

  | **returned**: success
  | **type**: dict
  | **sample**:

    .. code-block:: json

        {
            "acceptable-status": [
                "active"
            ],
            "access-basic-counter-set": true,
            "access-basic-sampling": false,
            "access-coprocessor-group-set": false,
            "access-crypto-activity-counter-set": true,
            "access-diagnostic-sampling": false,
            "access-extended-counter-set": true,
            "access-global-performance-data": true,
            "access-problem-state-counter-set": true,
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
            "crypto-configuration": {
                "crypto-adapter-uris": [
                    "/api/adapters/f1b97ed8-e578-11e8-a87c-00106f239c31"
                ],
                "crypto-domain-configurations": [
                    {
                        "access-mode": "control-usage",
                        "domain-index": 2
                    }
                ]
            },
            "current-cp-processing-weight": 1,
            "current-ifl-processing-weight": 1,
            "degraded-adapters": [],
            "description": "Colo dev partition",
            "has-unacceptable-status": false,
            "hba-uris": [],
            "hbas": [],
            "ifl-absolute-processor-capping": false,
            "ifl-absolute-processor-capping-value": 1.0,
            "ifl-processing-weight-capped": false,
            "ifl-processors": 12,
            "initial-cp-processing-weight": 100,
            "initial-ifl-processing-weight": 120,
            "initial-memory": 102400,
            "ipl-load-parameter": "",
            "is-locked": false,
            "maximum-cp-processing-weight": 999,
            "maximum-ifl-processing-weight": 999,
            "maximum-memory": 102400,
            "minimum-cp-processing-weight": 1,
            "minimum-ifl-processing-weight": 1,
            "name": "CSPF1",
            "nic-uris": [
                "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19/nics/5956e97a-f433-11ea-b67c-00106f239d19"
            ],
            "nics": [
                {
                    "adapter-id": "128",
                    "adapter-name": "OSD_128_MGMT_NET2_30",
                    "adapter-port": 0,
                    "class": "nic",
                    "description": "HAMGMT",
                    "device-number": "0004",
                    "element-id": "5956e97a-f433-11ea-b67c-00106f239d19",
                    "element-uri": "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19/nics/5956e97a-f433-11ea-b67c-00106f239d19",
                    "mac-address": "02:d2:4d:80:b9:88",
                    "name": "HAMGMT0",
                    "parent": "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19",
                    "ssc-ip-address": null,
                    "ssc-ip-address-type": null,
                    "ssc-management-nic": false,
                    "ssc-mask-prefix": null,
                    "type": "osd",
                    "virtual-switch-uri": "/api/virtual-switches/db2f0bec-e578-11e8-bd0a-00106f239c31",
                    "vlan-id": null,
                    "vlan-type": null
                }
            ],
            "object-id": "32323df4-f433-11ea-b67c-00106f239d19",
            "object-uri": "/api/partitions/32323df4-f433-11ea-b67c-00106f239d19",
            "os-name": "SSC",
            "os-type": "SSC",
            "os-version": "3.13.0",
            "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
            "partition-id": "08",
            "permit-aes-key-import-functions": true,
            "permit-cross-partition-commands": false,
            "permit-des-key-import-functions": true,
            "processor-management-enabled": false,
            "processor-mode": "shared",
            "reserve-resources": false,
            "reserved-memory": 0,
            "short-name": "CSPF1",
            "ssc-boot-selection": "appliance",
            "ssc-dns-servers": [
                "8.8.8.8"
            ],
            "ssc-host-name": "cpca-cspf1",
            "ssc-ipv4-gateway": null,
            "ssc-ipv6-gateway": null,
            "ssc-master-userid": "hmREST",
            "status": "active",
            "storage-group-uris": [
                "/api/storage-groups/4947c6d0-f433-11ea-8f73-00106f239d19"
            ],
            "threads-per-processor": 2,
            "type": "ssc",
            "virtual-function-uris": [],
            "virtual-functions": []
        }

  name
    Partition name

    | **type**: str

  {property}
    Additional properties of the partition, as described in the data model of the 'Partition' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.


  hbas
    HBAs of the partition. If the CPC does not have the storage-management feature enabled (ie. before z15), the list is empty.

    | **type**: list
    | **elements**: dict

    name
      HBA name

      | **type**: str

    {property}
      Additional properties of the HBA, as described in the data model of the 'HBA' element object of the 'Partition' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.



  nics
    NICs of the partition.

    | **type**: list
    | **elements**: dict

    name
      NIC name

      | **type**: str

    {property}
      Additional properties of the NIC, as described in the data model of the 'NIC' element object of the 'Partition' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.



  virtual-functions
    Virtual functions of the partition.

    | **type**: list
    | **elements**: dict

    name
      Virtual function name

      | **type**: str

    {property}
      Additional properties of the virtual function, as described in the data model of the 'Virtual Function' element object of the 'Partition' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.




