
:github_url: https://github.com/ansible-collections/ibm_zos_core/blob/dev/plugins/modules/zhmc_password_rule.py

.. _zhmc_password_rule_module:


zhmc_password_rule -- Create HMC password rules
===============================================



.. contents::
   :local:
   :depth: 1


Synopsis
--------
- Gather facts about a password rule on an HMC of a Z system.
- Create, delete, or update a password rule on an HMC.


Requirements
------------

- Access to the WS API of the HMC of the targeted Z system (see :term:`HMC API`).
- The targeted Z system can be in any operational mode (classic, DPM)




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
  The name of the target password rule.

  | **required**: True
  | **type**: str


state
  The desired state for the HMC password rule. All states are fully idempotent within the limits of the properties that can be changed:

  * ``absent``: Ensures that the password rule does not exist.

  * ``present``: Ensures that the password rule exists and has the specified properties.

  * ``facts``: Returns the password rule properties.

  | **required**: True
  | **type**: str
  | **choices**: absent, present, facts


properties
  Dictionary with desired properties for the password rule. Used for ``state=present``; ignored for ``state=absent|facts``. Dictionary key is the property name with underscores instead of hyphens, and dictionary value is the property value in YAML syntax. Integer properties may also be provided as decimal strings.

  The possible input properties in this dictionary are the properties defined as writeable in the data model for Password Rule resources (where the property names contain underscores instead of hyphens), with the following exceptions:

  * ``name``: Cannot be specified because the name has already been specified in the ``name`` module parameter.

  Properties omitted in this dictionary will remain unchanged when the password rule already exists, and will get the default value defined in the data model for password rules in the :term:`HMC API` when the password rule is being created.

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

   - name: Gather facts about a password rule
     zhmc_password_rule:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_password_rule_name }}"
       state: facts
     register: rule1

   - name: Ensure the password rule does not exist
     zhmc_password_rule:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_password_rule_name }}"
       state: absent

   - name: Ensure the password rule exists and has certain properties
     zhmc_password_rule:
       hmc_host: "{{ my_hmc_host }}"
       hmc_auth: "{{ my_hmc_auth }}"
       name: "{{ my_password_rule_name }}"
       state: present
       properties:
         description: "Example password rule 1"
     register: rule1











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

password_rule
  For ``state=absent``, an empty dictionary.

  For ``state=present|facts``, a dictionary with the resource properties of the target password rule.

  | **returned**: success
  | **type**: dict
  | **sample**:

    .. code-block:: json

        {
            "case-sensitive": false,
            "character-rules": [
                {
                    "alphabetic": "allowed",
                    "custom-character-sets": [],
                    "max-characters": 1,
                    "min-characters": 1,
                    "numeric": "not-allowed",
                    "special": "allowed"
                },
                {
                    "alphabetic": "required",
                    "custom-character-sets": [],
                    "max-characters": 28,
                    "min-characters": 4,
                    "numeric": "allowed",
                    "special": "allowed"
                },
                {
                    "alphabetic": "allowed",
                    "custom-character-sets": [],
                    "max-characters": 1,
                    "min-characters": 1,
                    "numeric": "not-allowed",
                    "special": "allowed"
                }
            ],
            "class": "password-rule",
            "consecutive-characters": 2,
            "description": "Standard password rule definition",
            "element-id": "520c0138-4a7e-11e9-8bb3-bdfeb245fc36",
            "element-uri": "/api/console/password-rules/520c0138-4a7e-11e9-8bb3-bdfeb245fc36",
            "expiration": 186,
            "history-count": 4,
            "max-length": 30,
            "min-length": 6,
            "name": "Standard",
            "parent": "/api/console",
            "replication-overwrite-possible": false,
            "similarity-count": 0,
            "type": "system-defined"
        }

  name
    Password rule name

    | **type**: str

  {property}
    Additional properties of the password rule, as described in the data model of the 'Password Rule' object in the :term:`HMC API` book. The property names have hyphens (-) as described in that book.



