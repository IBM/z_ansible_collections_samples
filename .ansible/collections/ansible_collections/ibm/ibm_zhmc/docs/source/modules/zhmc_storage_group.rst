
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_storage_group.py

.. _zhmc_storage_group_module:


zhmc_storage_group -- Create storage groups
===========================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Gather facts about a storage group associated with a CPC (Z system), including its storage volumes and virtual storage resources.
- Create, delete, or update a storage group associated with a CPC.


Requirements
------------

- Access to the WS API of the HMC of the targeted Z system (see :term:`HMC API`).
- The targeted Z system must be of generation z14 or later (to have the "dpm-storage-management" firmware feature) and must be in the Dynamic Partition Manager (DPM) operational mode.




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
  The name of the CPC associated with the target storage group.

  | **required**: True
  | **type**: str


name
  The name of the target storage group.

  | **required**: True
  | **type**: str


state
  The desired state for the storage group. All states are fully idempotent within the limits of the properties that can be changed, unless otherwise stated:

  * ``absent``: Ensures that the storage group does not exist. If the storage group is currently attached to any partitions, the module will fail (this is an idempotency limitation).

  * ``present``: Ensures that the storage group exists and is associated with the specified CPC, and has the specified properties. The attachment state of an already existing storage group to a partition is not changed.

  * ``facts``: Returns the storage group properties.

  | **required**: True
  | **type**: str
  | **choices**: absent, present, facts


properties
  Dictionary with desired properties for the storage group. Used for ``state=present``; ignored for ``state=absent|facts``. Dictionary key is the property name with underscores instead of hyphens, and dictionary value is the property value in YAML syntax. Integer properties may also be provided as decimal strings.

  The possible input properties in this dictionary are the properties defined as writeable in the data model for Storage Group resources (where the property names contain underscores instead of hyphens), with the following exceptions:

  * ``name``: Cannot be specified because the name has already been specified in the ``name`` module parameter.

  * ``type``: Cannot be changed once the storage group exists.

  Properties omitted in this dictionary will remain unchanged when the storage group already exists, and will get the default value defined in the data model for storage groups in the :term:`HMC API` when the storage group is being created.

  | **required**: False
  | **type**: dict


expand
  Boolean that controls whether the returned storage group contains additional artificial properties that expand certain URI or name properties to the full set of resource properties (see description of return values of this module).

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

   - name: Gather facts about a storage group
     zhmc_storage_group:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_storage_group_name }}"
       state: facts
       expand: true
     register: sg1

   - name: Ensure the storage group does not exist
     zhmc_storage_group:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_storage_group_name }}"
       state: absent

   - name: Ensure the storage group exists
     zhmc_storage_group:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_storage_group_name }}"
       state: present
       expand: true
       properties:
         description: "Example storage group 1"
         type: fcp
         shared: false
         connectivity: 4
         max-partitions: 1
     register: sg1





Notes
-----

.. note::
   This module manages only the knowledge of the Z system about its storage, but does not perform any actions against the storage subsystems or SAN switches attached to the Z system.

   Attachment of storage groups to and from partitions is managed by the Ansible module zhmc_storage_group_attachment.







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

storage_group
  For ``state=absent``, an empty dictionary.

  For ``state=present|facts``, the resource properties of the target storage group after any changes, plus additional artificial properties as described below.

  | **returned**: success
  | **type**: dict
  | **sample**:

    .. code-block:: json

        {
            "active-connectivity": 6,
            "active-max-partitions": 1,
            "attached-partition-names": [
                "MGMT1"
            ],
            "attached-partitions": [
                {
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
                    "crypto-configuration": {},
                    "current-cp-processing-weight": 1,
                    "current-ifl-processing-weight": 1,
                    "degraded-adapters": [],
                    "description": "Colo dev partition",
                    "has-unacceptable-status": false,
                    "hba-uris": [],
                    "ifl-absolute-processor-capping": false,
                    "ifl-absolute-processor-capping-value": 1.0,
                    "ifl-processing-weight-capped": false,
                    "ifl-processors": 4,
                    "initial-cp-processing-weight": 100,
                    "initial-ifl-processing-weight": 100,
                    "initial-memory": 68608,
                    "ipl-load-parameter": "",
                    "is-locked": false,
                    "maximum-cp-processing-weight": 999,
                    "maximum-ifl-processing-weight": 999,
                    "maximum-memory": 68608,
                    "minimum-cp-processing-weight": 1,
                    "minimum-ifl-processing-weight": 1,
                    "name": "MGMT1",
                    "nic-uris": [],
                    "object-id": "009c0f4c-3588-11e9-bad3-00106f239d19",
                    "object-uri": "/api/partitions/009c0f4c-3588-11e9-bad3-00106f239d19",
                    "os-name": "SSC",
                    "os-type": "SSC",
                    "os-version": "3.13.0",
                    "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                    "partition-id": "00",
                    "permit-aes-key-import-functions": true,
                    "permit-cross-partition-commands": false,
                    "permit-des-key-import-functions": true,
                    "processor-management-enabled": false,
                    "processor-mode": "shared",
                    "reserve-resources": false,
                    "reserved-memory": 0,
                    "short-name": "MGMT1",
                    "ssc-boot-selection": "appliance",
                    "ssc-dns-servers": [
                        "8.8.8.8"
                    ],
                    "ssc-host-name": "cpca-mgmt1",
                    "ssc-ipv4-gateway": "172.16.192.1",
                    "ssc-ipv6-gateway": null,
                    "ssc-master-userid": "hmREST",
                    "status": "active",
                    "storage-group-uris": [
                        "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31"
                    ],
                    "threads-per-processor": 2,
                    "type": "ssc",
                    "virtual-function-uris": []
                }
            ],
            "candidate-adapter-port-uris": [
                "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31/storage-ports/0"
            ],
            "candidate-adapter-ports": [
                {
                    "class": "storage-port",
                    "description": "",
                    "element-id": "0",
                    "element-uri": "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31/storage-ports/0",
                    "fabric-id": "100088947155A1E9",
                    "index": 0,
                    "name": "Port 0",
                    "parent": "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31",
                    "parent-adapter": {
                        "adapter-family": "ficon",
                        "adapter-id": "124",
                        "allowed-capacity": 64,
                        "card-location": "A14B-D113-J.01",
                        "channel-path-id": "08",
                        "class": "adapter",
                        "configured-capacity": 14,
                        "description": "",
                        "detected-card-type": "ficon-express-16s-plus",
                        "maximum-total-capacity": 254,
                        "name": "FCP_124_SAN1_03",
                        "object-id": "e03d413a-e578-11e8-a87c-00106f239c31",
                        "object-uri": "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31",
                        "parent": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                        "physical-channel-status": "operating",
                        "port-count": 1,
                        "state": "online",
                        "status": "active",
                        "storage-port-uris": [
                            "/api/adapters/e03d413a-e578-11e8-a87c-00106f239c31/storage-ports/0"
                        ],
                        "type": "fcp",
                        "used-capacity": 18
                    }
                }
            ],
            "class": "storage-group",
            "connectivity": 6,
            "cpc-uri": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
            "description": "Storage group for partition MGMT1",
            "direct-connection-count": 0,
            "fulfillment-state": "complete",
            "max-partitions": 1,
            "name": "CPCA_SG_MGMT1",
            "object-id": "edd782f2-200a-11e9-a142-00106f239c31",
            "object-uri": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31",
            "parent": "/api/console",
            "shared": false,
            "storage-volume-uris": [
                "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/storage-volumes/f02e2632-200a-11e9-8748-00106f239c31"
            ],
            "storage-volumes": [
                {
                    "active-size": 128.0,
                    "class": "storage-volume",
                    "description": "Boot volume",
                    "element-id": "f02e2632-200a-11e9-8748-00106f239c31",
                    "element-uri": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/storage-volumes/f02e2632-200a-11e9-8748-00106f239c31",
                    "fulfillment-state": "complete",
                    "name": "MGMT1_MGMT1-boot",
                    "parent": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31",
                    "paths": [
                        {
                            "device-number": "0015",
                            "logical-unit-number": "0000000000000000",
                            "partition-uri": "/api/partitions/009c0f4c-3588-11e9-bad3-00106f239d19",
                            "target-world-wide-port-name": "5005076810260382"
                        }
                    ],
                    "size": 128.0,
                    "usage": "boot",
                    "uuid": "600507681081001D4800000000000083"
                }
            ],
            "type": "fcp",
            "unassigned-world-wide-port-names": [],
            "virtual-storage-resource-uris": [
                "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/virtual-storage-resources/db682456-358a-11e9-bc93-00106f239d19"
            ],
            "virtual-storage-resources": [
                {
                    "adapter-port-uri": "/api/adapters/e0ea33d6-e578-11e8-a87c-00106f239c31/storage-ports/0",
                    "class": "virtual-storage-resource",
                    "description": "",
                    "device-number": "0015",
                    "element-id": "db682456-358a-11e9-bc93-00106f239d19",
                    "element-uri": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31/virtual-storage-resources/db682456-358a-11e9-bc93-00106f239d19",
                    "name": "vhba_CPCA_SG_MGMT12",
                    "parent": "/api/storage-groups/edd782f2-200a-11e9-a142-00106f239c31",
                    "partition-uri": "/api/partitions/009c0f4c-3588-11e9-bad3-00106f239d19",
                    "world-wide-port-name": "c05076d24d80016e",
                    "world-wide-port-name-info": {
                        "status": "validated",
                        "world-wide-port-name": "c05076d24d80016e"
                    }
                }
            ]
        }

  name
    Storage group name

    | **type**: str

  {property}
    Additional properties of the storage group, as described in the data model of the 'Storage Group' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.


  attached-partition-names
    Names of the partitions to which the storage group is attached.

    | **type**: list
    | **elements**: str

  candidate-adapter-ports
    Only present if ``expand=true``: List of candidate storage adapter ports of the storage group.

    | **returned**: success+expand
    | **type**: list
    | **elements**: dict

    name
      Storage port name

      | **type**: str

    index
      Storage port index

      | **type**: int

    {property}
      Additional properties of the storage port, as described in the data model of the 'Storage Port' element object of the 'Adapter' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.


    parent-adapter
      Storage adapter of the candidate port.

      | **type**: dict

      name
        Storage adapter name

        | **type**: str

      {property}
        Additional properties of the storage adapter, as described in the data model of the 'Adapter' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.




  storage-volumes
    Only present if ``expand=true``: Storage volumes of the storage group.

    | **returned**: success+expand
    | **type**: list
    | **elements**: dict

    name
      Storage volume name

      | **type**: str

    {property}
      Additional properties of the storage volume, as described in the data model of the 'Storage Volume' element object of the 'Storage Group' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.



  virtual-storage-resources
    Only present if ``expand=true``: Virtual storage resources of the storage group.

    | **returned**: success+expand
    | **type**: list
    | **elements**: dict

    {property}
      Properties of the virtual storage resource, as described in the data model of the 'Virtual Storage Resource' element object of the 'Storage Group' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.



  attached-partitions
    Only present if ``expand=true``: Partitions to which the storage group is attached.

    | **returned**: success+expand
    | **type**: list
    | **elements**: dict

    {property}
      Properties of the partition, as described in the data model of the 'Partition' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.




