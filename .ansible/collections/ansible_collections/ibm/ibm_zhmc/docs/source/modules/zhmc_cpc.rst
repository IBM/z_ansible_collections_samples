
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_cpc.py

.. _zhmc_cpc_module:


zhmc_cpc -- Manage CPCs
=======================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Deactivate a CPC (Z system).
- Activate a CPC and update its properties.
- Gather facts about a CPC, and for DPM operational mode, including its adapters, partitions and storage groups.
- Update the properties of a CPC.


Requirements
------------

- Access to the WS API of the HMC of the targeted CPC (see :term:`HMC API`).
- The targeted CPC can be in any operational mode (classic, DPM).




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



name
  The name of the target CPC.

  | **required**: True
  | **type**: str


state
  The desired state for the CPC. All states are fully idempotent within the limits of the properties that can be changed:

  * ``inactive``: Ensures the CPC is inactive.

  * ``active``: Ensures the CPC is active and then ensures that the CPC has the specified properties. The operational mode of the CPC cannot be changed.

  * ``set``: Ensures that the CPC has the specified properties.

  * ``facts``: Returns the CPC properties including its child resources.

  | **required**: True
  | **type**: str
  | **choices**: inactive, active, set, facts


activation_profile_name
  The name of the reset activation profile to be used when activating the CPC in the classic operational mode, for ``state=active``. This parameter is ignored when the CPC is in classic mode and was already active, and when the CPC is in DPM mode.

  Default: The reset activation profile specified in the 'next-activation-profile-name' property of the CPC.

  This parameter is not allowed for the other ``state`` values.

  | **required**: False
  | **type**: str


properties
  Only for ``state=set`` and ``state=active``: New values for the properties of the CPC. Properties omitted in this dictionary will remain unchanged. This parameter will be ignored for other ``state`` values.

  The parameter is a dictionary. The key of each dictionary item is the property name as specified in the data model for CPC resources, with underscores instead of hyphens. The value of each dictionary item is the property value (in YAML syntax). Integer properties may also be provided as decimal strings.

  The possible properties in this dictionary are the properties defined as writeable in the data model for CPC resources.

  | **required**: False
  | **type**: dict


log_file
  File path of a log file to which the logic flow of this module as well as interactions with the HMC are logged. If null, logging will be propagated to the Python root logger.

  | **required**: False
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   ---
   # Note: The following examples assume that some variables named 'my_*' are set.

   - name: Gather facts about the CPC
     zhmc_cpc:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_cpc_name }}"
       state: facts
     register: cpc1

   - name: Ensure the CPC is inactive
     zhmc_cpc:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_cpc_name }}"
       state: inactive

   - name: Ensure the CPC is active
     zhmc_cpc:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_cpc_name }}"
       state: active
     register: cpc1

   - name: Ensure the CPC has the desired property values
     zhmc_cpc:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_cpc_name }}"
       state: set
       properties:
         acceptable_status:
          - active
         description: "This is CPC {{ my_cpc_name }}"
     register: cpc1











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

cpc
  The CPC and its adapters, partitions, and storage groups.

  | **returned**: success
  | **type**: dict
  | **sample**:

    .. code-block:: json

        {
            "adapters": [
                {
                    "adapter-family": "ficon",
                    "adapter-id": "120",
                    "name": "FCP_120_SAN1_02",
                    "object-uri": "/api/adapters/dfb2147a-e578-11e8-a87c-00106f239c31",
                    "status": "active",
                    "type": "fcp"
                },
                {
                    "adapter-family": "osa",
                    "adapter-id": "10c",
                    "name": "OSM1",
                    "object-uri": "/api/adapters/ddde026c-e578-11e8-a87c-00106f239c31",
                    "status": "active",
                    "type": "osm"
                }
            ],
            "name": "CPCA",
            "partitions": [
                {
                    "name": "PART1",
                    "object-uri": "/api/partitions/c44338de-351b-11e9-9fbb-00106f239d19",
                    "status": "stopped",
                    "type": "linux"
                },
                {
                    "name": "PART2",
                    "object-uri": "/api/partitions/6a46d18a-cf79-11e9-b447-00106f239d19",
                    "status": "active",
                    "type": "ssc"
                }
            ],
            "storage-groups": [
                {
                    "cpc-uri": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                    "fulfillment-state": "complete",
                    "name": "CPCA_SG_PART1",
                    "object-uri": "/api/storage-groups/58e41a42-20a6-11e9-8dfc-00106f239c31",
                    "type": "fcp"
                },
                {
                    "cpc-uri": "/api/cpcs/66942455-4a14-3f99-8904-3e7ed5ca28d7",
                    "fulfillment-state": "complete",
                    "name": "CPCA_SG_PART2",
                    "object-uri": "/api/storage-groups/4947c6d0-f433-11ea-8f73-00106f239d19",
                    "type": "fcp"
                }
            ],
            "{property}": "... more properties ... "
        }

  name
    CPC name

    | **type**: str

  {property}
    Additional properties of the CPC, as described in the data model of the 'CPC' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.


  adapters
    The adapters of the CPC, with a subset of their properties. For details, see the :term:`HMC API` book.

    | **type**: list
    | **elements**: dict

    name
      Adapter name

      | **type**: str

    object-uri
      Canonical URI of the adapter

      | **type**: str

    adapter-id
      Adapter ID (PCHID)

      | **type**: str

    type
      Adapter type

      | **type**: str

    adapter-family
      Adapter family

      | **type**: str

    status
      Status of the adapter

      | **type**: str


  partitions
    The defined partitions of the CPC, with a subset of their properties. For details, see the :term:`HMC API` book.

    | **type**: list
    | **elements**: dict

    name
      Partition name

      | **type**: str

    object-uri
      Canonical URI of the partition

      | **type**: str

    type
      Type of the partition

      | **type**: str

    status
      Status of the partition

      | **type**: str


  storage-groups
    The storage groups associated with the CPC, with a subset of their properties. For details, see the :term:`HMC API` book.

    | **type**: list
    | **elements**: dict

    name
      Storage group name

      | **type**: str

    object-uri
      Canonical URI of the storage group

      | **type**: str

    type
      Storage group type

      | **type**: str

    fulfillment-status
      Fulfillment status of the storage group

      | **type**: str

    cpc-uri
      Canonical URI of the associated CPC

      | **type**: str



