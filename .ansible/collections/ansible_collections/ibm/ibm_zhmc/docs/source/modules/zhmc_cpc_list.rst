
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_cpc_list.py

.. _zhmc_cpc_list_module:


zhmc_cpc_list -- List CPCs
==========================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- List CPCs (Z systems). By default, only the CPCs managed by the targeted HMC are listed. Optionally, unmanaged CPCs can be listed in addition.


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



include_unmanaged_cpcs
  Include unmanaged CPCs in the result. The unmanaged CPCs will have only their name as a property. Note that managed CPCs are always included in the result.

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

   - name: List managed CPCs
     zhmc_cpc_list:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
     register: cpc_list

   - name: List managed and unmanaged CPCs
     zhmc_cpc_list:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       include_unmanaged_cpcs: true
     register: cpc_list











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

cpcs
  The list of CPCs, with a subset of their properties.

  | **returned**: success
  | **type**: list
  | **elements**: dict
  | **sample**:

    .. code-block:: json

        [
            {
                "dpm_enabled": true,
                "has_unacceptable_status": false,
                "is_managed": true,
                "name": "CPCA",
                "se_version": "2.15",
                "status": "active"
            },
            {
                "is_managed": false,
                "name": "NewCPC"
            }
        ]

  name
    CPC name

    | **type**: str

  is_managed
    Indicates wehether the CPC is managed by the targeted HMC (true) or is unmanaged (false).

    | **type**: bool

  status
    The current status of the CPC. For details, see the description of the 'status' property in the data model of the 'CPC' resource (see :term:`HMC API`). Only included for managed CPCs.

    | **type**: str

  has_unacceptable_status
    Indicates whether the current status of the CPC is unacceptable, based on its 'acceptable-status' property. Only included for managed CPCs.

    | **type**: bool

  dpm_enabled
    Indicates wehether the CPC is in DPM mode (true) or in classic mode (false). Only included for managed CPCs.

    | **type**: bool

  se_version
    The SE version of the CPC, as a string 'M.N.U'. Only included for managed CPCs.

    | **type**: str


