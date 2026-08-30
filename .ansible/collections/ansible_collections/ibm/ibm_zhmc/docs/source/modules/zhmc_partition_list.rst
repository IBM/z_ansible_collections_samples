
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_partition_list.py

.. _zhmc_partition_list_module:


zhmc_partition_list -- List partitions
======================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- List partitions on a specific CPC (Z system) or on all managed CPCs.
- CPCs in classic mode are ignored (i.e. do not lead to a failure).
- Partitions for which the user has no object access permission are ignored (i.e. do not lead to a failure).
- The module works for any HMC version. On HMCs with version 2.14.0 or higher, the "List Permitted Partitions" opration is used. On older HMCs, the managed CPCs are listed and the partitions on each CPC.


Requirements
------------

- Access to the WS API of the HMC (see :term:`HMC API`).




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
  Name of the CPC for which the partitions are to be listed.

  Default: All managed CPCs.

  | **required**: False
  | **type**: str


log_file
  File path of a log file to which the logic flow of this module as well as interactions with the HMC are logged. If null, logging will be propagated to the Python root logger.

  | **required**: False
  | **type**: str




Examples
--------

.. code-block:: yaml+jinja

   
   ---
   # Note: The following examples assume that some variables named 'my_*' are set.

   - name: List the permitted partitions on all managed CPCs
     zhmc_partition_list:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
     register: partition_list

   - name: List the permitted partitions on a CPC
     zhmc_partition_list:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: CPCA
     register: partition_list











Return Values
-------------


changed
  Indicates if any change has been made by the module. This will always be false.

  | **returned**: always
  | **type**: bool

msg
  An error message that describes the failure.

  | **returned**: failure
  | **type**: str

partitions
  The list of permitted partitions, with a subset of their properties.

  | **returned**: success
  | **type**: list
  | **elements**: dict
  | **sample**:

    .. code-block:: json

        [
            {
                "cpc_name": "CPC1",
                "has_unacceptable_status": false,
                "name": "partition1",
                "se_version": "2.15.0",
                "status": "active"
            }
        ]

  name
    partition name

    | **type**: str

  cpc_name
    Name of the parent CPC of the partition

    | **type**: str

  se_version
    SE version of the parent CPC of the partition

    | **type**: str

  status
    The current status of the partition. For details, see the description of the 'status' property in the data model of the 'Logical Partition' resource (see :term:`HMC API`).

    | **type**: str

  has_unacceptable_status
    Indicates whether the current status of the partition is unacceptable, based on its 'acceptable-status' property.

    | **type**: bool


