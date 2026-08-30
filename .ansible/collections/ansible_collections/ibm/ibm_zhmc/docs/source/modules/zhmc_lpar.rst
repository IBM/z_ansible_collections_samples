
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_lpar.py

.. _zhmc_lpar_module:


zhmc_lpar -- Manage LPARs
=========================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Gather facts about an LPAR of a CPC (Z system) in classic mode.
- Update modifiable properties of an active LPAR.
- Activate an LPAR and update its properties.
- Load an LPAR and update its properties.
- Deactivate an LPAR using the 'Deactivate Logical Partition' operation.
- Initialize for load using the 'Reset Clear' or 'Reset Normal' operations.


Requirements
------------

- Access to the WS API of the HMC of the targeted CPC (see :term:`HMC API`).
- The targeted CPC must be in the classic operational mode.
- The HMC userid must have the following HMC permissions:
- Object-access permission to the target LPAR and its parent CPC.
- If the 'next-activation-profile-name' property is to be updated, task permission for the 'Change Object Options' task or the 'Customize/Delete Activation Profiles' task.
- If any of the 'zaware-...' properties is to be updated, task permission for the 'Firmware Details' task.
- If any of the numbers of allocated or reserved cores is is to be updated, task permission for the 'Logical Processor Add' task.
- If the LPAR needs to be activated, task permission for the 'Activate' task.
- If the LPAR needs to be deactivated, task permission for the 'Deactivate' task.




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
  The name of the CPC with the target LPAR.

  | **required**: True
  | **type**: str


name
  The name of the target LPAR.

  | **required**: True
  | **type**: str


state
  The desired state for the LPAR:

  * ``inactive``: Ensures that the LPAR is inactive (i.e. status 'not-activated'), unless the LPAR is currently operating and the ``force`` parameter was not set to True. Properties cannot be updated. The LPAR is deactivated if needed.

  * ``reset_clear``: Initialize the LPAR for loading by performing a 'Reset Clear' operation (clearing its pending interruptions, resetting its channel subsystem, resetting its processors, clearing its memory), unless the LPAR is currently loaded (i.e. status is 'operating' or 'acceptable') and the ``force`` parameter was not set to True. Properties cannot be updated. After successful execution of the 'Reset Normal' operation, the LPAR will be inactive (i.e. status 'not-activated').

  * ``reset_normal``: Initialize the LPAR for loading by performing a 'Reset Normal' operation (clearing its pending interruptions, resetting its channel subsystem, resetting its processors), unless the LPAR is currently loaded (i.e. status is 'operating' or 'acceptable') and the ``force`` parameter was not set to True. Properties cannot be updated. After successful execution of the 'Reset Normal' operation, the LPAR  will be inactive (i.e. status 'not-activated').

  * ``active``: Ensures that the LPAR is at least active (i.e. status is 'not-operating', 'operating' or 'acceptable'), and then ensures that the LPAR properties have the specified values. The LPAR is activated if needed. If auto-load is set in the activation profile, the LPAR will also be loaded.

  * ``loaded``: Ensures that the LPAR is loaded (i.e. status is 'operating' or 'acceptable'), and then ensures that the LPAR properties have the specified values. The LPAR is first activated if needed, and then loaded if needed.

  * ``set``: Ensures that the LPAR properties have the specified values. Requires that the LPAR is at least active (i.e. status is 'not-operating', 'operating' or 'acceptable') but does not activate the LPAR if that is not the case.

  * ``facts``: Returns the current LPAR properties.

  In all cases, the LPAR must exist.

  | **required**: True
  | **type**: str
  | **choices**: inactive, reset_clear, reset_normal, active, loaded, set, facts


activation_profile_name
  The name of the image or load activation profile to be used when the LPAR needs to be activated, for ``state=active`` and ``state=loaded``.

  Default: The image or load activation profile specified in the 'next-activation-profile-name' property of the LPAR is used when the LPAR needs to be activated.

  If the LPAR was already active, the ``force`` parameter determines what happens.

  This parameter is not allowed for the other ``state`` values.

  | **required**: False
  | **type**: str


force
  Controls whether operations that change the LPAR status are performed when the LPAR is currently loaded (i.e. status 'operating' or 'acceptable'):

  If True, such operations are performed regardless of the current LPAR status.

  If False, such operations are performed only if the LPAR is not currently loaded, and are rejected otherwise.

  | **required**: False
  | **type**: bool


os_ipl_token
  Setting this parameter for ``state=reset_clear`` or ``state=reset_normal`` requests that the corresponding HMC operations only be performed if the provided value matches the current value of the 'os-ipl-token' property of the LPAR, and be rejected otherwise. Note that the 'os-ipl-token' property of the LPAR is set by the operating system and is set only by some operating systems, such as z/OS. This parameter is ignored for other ``state`` values.

  | **required**: False
  | **type**: str


properties
  Dictionary with new values for the LPAR properties, for ``state=active``, ``state=loaded`` and ``state=set``. Key is the property name with underscores instead of hyphens, and value is the property value in YAML syntax. Integer properties may also be provided as decimal strings.

  The possible input properties in this dictionary are the properties defined as writeable in the data model for LPAR resources (where the property names contain underscores instead of hyphens).

  Properties omitted in this dictionary will not be updated.

  This parameter is not allowed for the other ``state`` values.

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

   - name: Ensure the LPAR is inactive
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: inactive
     register: lpar1

   - name: Ensure the LPAR is active (using the default image profile when it needs to be activated), and then set the CP sharing weight to 20
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: active
       properties:
         initial_processing_weight: 20
     register: lpar1

   - name: Ensure the LPAR is active (using image profile LPAR2 when it needs to be activated)
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: active
       activation_profile_name: LPAR2
     register: lpar1

   - name: Ensure the LPAR is loaded (using the default image profile when it needs to be activated)
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: loaded
     register: lpar1

   - name: Ensure the LPAR is initialized for loading, clearing its memory
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: reset_clear
     register: lpar1

   - name: Ensure the LPAR is initialized for loading, not clearing its memory
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: reset_normal
     register: lpar1

   - name: Ensure the CP sharing weight of the LPAR is 30
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: set
       properties:
         initial_processing_weight: 30
     register: lpar1

   - name: Gather facts about the LPAR
     zhmc_lpar:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       cpc_name: "{{ my_cpc_name }}"
       name: "{{ my_lpar_name }}"
       state: facts
     register: lpar1











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

lpar
  The resource properties of the LPAR, after any specified updates have been applied.

  Note that the returned properties may show different values than the ones that were specified as input for the update. For example, memory properties may be rounded up, hexadecimal strings may be shown with a different representation format, and other properties may change as a result of updating some properties. For details, see the data model of the 'Logical Partition' object in the :term:`HMC API` book.

  | **returned**: success
  | **type**: dict
  | **sample**:

    .. code-block:: json

        {
            "absolute-aap-capping": {
                "type": "none"
            },
            "absolute-cbp-capping": {
                "type": "none"
            },
            "absolute-cf-capping": {
                "type": "none"
            },
            "absolute-ifl-capping": {
                "type": "none"
            },
            "absolute-processing-capping": {
                "type": "none"
            },
            "absolute-ziip-capping": {
                "type": "none"
            },
            "acceptable-status": [
                "operating"
            ],
            "activation-mode": "ssc",
            "additional-status": "",
            "class": "logical-partition",
            "cluster-name": "",
            "current-aap-processing-weight": null,
            "current-aap-processing-weight-capped": null,
            "current-cbp-processing-weight": null,
            "current-cbp-processing-weight-capped": null,
            "current-cf-processing-weight": null,
            "current-cf-processing-weight-capped": null,
            "current-ifl-processing-weight": null,
            "current-ifl-processing-weight-capped": null,
            "current-processing-weight": 10,
            "current-processing-weight-capped": false,
            "current-vfm-storage": 0,
            "current-ziip-processing-weight": null,
            "current-ziip-processing-weight-capped": null,
            "defined-capacity": 0,
            "description": "LPAR Image",
            "group-profile-capacity": null,
            "group-profile-uri": null,
            "has-operating-system-messages": true,
            "has-unacceptable-status": false,
            "initial-aap-processing-weight": null,
            "initial-aap-processing-weight-capped": null,
            "initial-cbp-processing-weight": null,
            "initial-cbp-processing-weight-capped": null,
            "initial-cf-processing-weight": null,
            "initial-cf-processing-weight-capped": null,
            "initial-ifl-processing-weight": null,
            "initial-ifl-processing-weight-capped": null,
            "initial-processing-weight": 10,
            "initial-processing-weight-capped": false,
            "initial-vfm-storage": 0,
            "initial-ziip-processing-weight": null,
            "initial-ziip-processing-weight-capped": null,
            "is-locked": false,
            "last-used-activation-profile": "ANGEL",
            "last-used-boot-record-logical-block-address": "0",
            "last-used-disk-partition-id": 0,
            "last-used-load-address": "00000",
            "last-used-load-parameter": "",
            "last-used-logical-unit-number": "0",
            "last-used-operating-system-specific-load-parameters": "",
            "last-used-world-wide-port-name": "0",
            "maximum-aap-processing-weight": null,
            "maximum-cbp-processing-weight": null,
            "maximum-cf-processing-weight": null,
            "maximum-ifl-processing-weight": null,
            "maximum-processing-weight": 0,
            "maximum-vfm-storage": 0,
            "maximum-ziip-processing-weight": null,
            "minimum-aap-processing-weight": null,
            "minimum-cbp-processing-weight": null,
            "minimum-cf-processing-weight": null,
            "minimum-ifl-processing-weight": null,
            "minimum-processing-weight": 0,
            "minimum-ziip-processing-weight": null,
            "name": "ANGEL",
            "next-activation-profile-name": "ANGEL",
            "object-id": "10fa8489-4e06-3601-9170-eee82e26937c",
            "object-uri": "/api/logical-partitions/10fa8489-4e06-3601-9170-eee82e26937c",
            "os-ipl-token": "0000000000000000",
            "os-level": "1.0.0",
            "os-name": "INSTALL",
            "os-type": "SSC",
            "parent": "/api/cpcs/4f01576a-c3f6-3224-a951-b1bf361886a4",
            "partition-identifier": "33",
            "partition-number": "2f",
            "program-status-word-information": [
                {
                    "cpid": "00",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "01",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "02",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "03",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "04",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "05",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "06",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "07",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "08",
                    "psw": "0706C00180000000000000000070E050"
                },
                {
                    "cpid": "09",
                    "psw": "0706C00180000000000000000070E050"
                }
            ],
            "ssc-dns-info": null,
            "ssc-gateway-info": null,
            "ssc-host-name": null,
            "ssc-master-userid": null,
            "ssc-network-info": null,
            "status": "operating",
            "storage-central-allocation": [
                {
                    "current": 8192,
                    "gap": 102400,
                    "initial": 8192,
                    "maximum": 8192,
                    "origin": 127322112,
                    "storage-element-type": "central"
                }
            ],
            "storage-expanded-allocation": [],
            "sysplex-name": null,
            "workload-manager-enabled": false
        }

  name
    LPAR name

    | **type**: str

  {property}
    Additional properties of the LPAR, as described in the data model of the 'Logical Partition' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.



