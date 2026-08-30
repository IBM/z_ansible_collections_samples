#!/usr/bin/python
# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2020,2023
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function
__metaclass__ = type

DOCUMENTATION = r'''
---
module: cmci_get
short_description: Query CICS and CICSPlex SM resources and definitions
description:
  - Get information about installed and definitional CICS® and CICSPlex® SM
    resources from CICS regions, by initiating GET requests via the CMCI REST
    API. The CMCI REST API can be configured in CICSPlex SM or stand-alone
    regions (SMSS). For information about the API, see
    L(CMCI REST API,https://www.ibm.com/docs/en/cics-ts/latest?topic=programming-cmci-rest-api-reference).
    For information about how to compose GET requests, see
    L(CMCI GET requests,https://www.ibm.com/docs/en/cics-ts/latest?topic=requests-cmci-get).
version_added: 1.0.0
author:
  - Stewart Francis (@stewartfrancis)
  - Tom Latham (@Tom-Latham)
  - Sophie Green (@sophiegreen)
  - Ya Qing Chen (@vera-chan)
extends_documentation_fragment:
  - ibm.ibm_zos_cics.cmci.COMMON
  - ibm.ibm_zos_cics.cmci.RESOURCES
options:
  record_count:
    description:
      - Identifies a subset of records in the results cache, starting either
        from the first record in the results cache or from the record specified
        by the index parameter. If not specified, all the records are returned
        by default.
      - A negative number indicates a count back from the last record; for
        example, C(-1) means the last record, C(-2) the last record but one, and
        so on.
      - The count value must be an integer; a value of zero is not permitted.
    type: int
    required: false
  fail_on_nodata:
    description:
      - Specifies whether the module should fail if no data is returned by the
        query. If set to true, the module will fail if no data is returned.
      - Default behaviour is for the module to fail if no data is returned. When
        set to false, the module will return OK, just with no records.
    type: bool
    required: false
    default: true
'''


EXAMPLES = r"""
- name: get 2 LOCFILEs from a CICSplex
  cmci_get:
    cmci_host: "winmvs2c.hursley.ibm.com"
    cmci_port: 10080
    cmci_user: "ibmuser"
    cmci_password: "123456"
    context: "iyk3z0r9"
    type: CICSLocalFile
    record_count: 2
    resource:
      filter:
        dsname: "CTS*"

- name: get a localfile in a CICS region
  cmci_get:
    cmci_host: "winmvs2c.hursley.ibm.com"
    cmci_port: 10080
    cmci_cert: "./sec/ansible.pem"
    cmci_key: "./sec/ansible.key"
    context: "iyk3z0r9"
    type: "CICSLocalFile"
    resources:
      filter:
        dsname: "XIAOPIN*"
        file: "DFH*"
    record_count: 1

- name: get a progdef from a CSD
  cmci_get:
    cmci_host: "winmvs2c.hursley.ibm.com"
    cmci_port: 10080
    cmci_cert: "./sec/ansible.pem"
    cmci_key: "./sec/ansible.key"
    context: "iyk3z0r9"
    type: cicsdefinitionprogram
    resources:
      filter:
        name: MYPROG
      get_parameters:
        - name: csdgroup
          value: MYGRP
    record_count: 1

- name: pass module even if bundle definition is not found
  cmci_get:
    cmci_host: "winmvs2c.hursley.ibm.com"
    cmci_port: 10080
    cmci_cert: "./sec/ansible.pem"
    cmci_key: "./sec/ansible.key"
    context: "iyk3z0r9"
    type: cicsdefinitionbundle
    resources:
      filter:
        name: MYBUNDLE
      get_parameters:
        - name: csdgroup
          value: MYGRP
    record_count: 1
    fail_on_nodata: "false"

- name: Using complex_filter to combine filter expressions and change operators
  cmci_get:
    cmci_host: "winmvs2c.hursley.ibm.com"
    cmci_port: 10080
    cmci_cert: "./sec/ansible.pem"
    cmci_key: "./sec/ansible.key"
    context: "iyk3z0r9"
    type: "CICSRegion"
    resources:
      complex_filter:
        or:
          [
            { attribute: "currtasks", value: "10", operator: "<" },
            { attribute: "currtasks", value: "100", operator: ">" },
          ]
    record_count: 1
"""


RETURN = r"""
changed:
  description: True if the state was changed, otherwise False.
  returned: always
  type: bool
failed:
  description: True if the query job failed, otherwise False.
  returned: always
  type: bool
connect_version:
  description: Version of the CMCI API
  returned: success
  type: str
cpsm_reason:
  description:
    - The character value of the REASON code returned by each CICSPlex SM API
      command. For a list of REASON character values, see
      https://www.ibm.com/docs/en/cics-ts/latest?topic=values-eyuda-reason-in-alphabetical-order.
  returned: success
  type: str
cpsm_reason_code:
  description:
    - The numeric value of the REASON code returned by each CICSPlex SM API
      command. For a list of REASON numeric values, see
      https://www.ibm.com/docs/en/cics-ts/latest?topic=values-eyuda-reason-in-numerical-order.
  returned: success
  type: int
cpsm_response:
  description:
    - The character value of the RESPONSE code returned by each CICSPlex SM API
      command. For a list of RESPONSE character values, see
      https://www.ibm.com/docs/en/cics-ts/latest?topic=values-eyuda-response-in-alphabetical-order.
  returned: success
  type: str
cpsm_response_code:
  description:
    - The numeric value of the RESPONSE code returned by each CICSPlex SM API
      command. For a list of RESPONSE numeric values, see
      https://www.ibm.com/docs/en/cics-ts/latest?topic=values-eyuda-response-in-numerical-order.
  returned: success
  type: str
http_status:
  description:
    - The message associated with HTTP status code that is returned by CMCI.
  returned: success
  type: str
http_status_code:
  description:
    - The HTTP status code returned by CMCI.
  returned: success
  type: int
record_count:
  description:
    - The number of records returned.
  returned: success
  type: int
records:
  description:
    - A list of the returned records.
  returned: success
  type: list
  elements: dict
  sample:
    - _keydata: "C1D5E2C9E3C5E2E3"
      aloadtime: "00:00:00.000000"
      apist: "CICSAPI"
      application: ""
      applmajorver: "-1"
      applmicrover: "-1"
      applminorver: "-1"
      basdefinever: "0"
      cedfstatus: "CEDF"
      changeagent: "CSDAPI"
      changeagrel: "0730"
      changetime: "2020-12-15T02:34:31.000000+00:00"
      changeusrid: "YQCHEN"
      coboltype: "NOTAPPLIC"
      concurrency: "QUASIRENT"
      copy: "NOTREQUIRED"
      currentloc: "NOCOPY"
      datalocation: "ANY"
      definesource: "ANSITEST"
      definetime: "2020-12-15T02:34:29.000000+00:00"
      dynamstatus: "NOTDYNAMIC"
      entrypoint: "FF000000"
      execkey: "USEREXECKEY"
      executionset: "FULLAPI"
      eyu_cicsname: "IYCWEMW2"
      eyu_cicsrel: "E730"
      eyu_reserved: "0"
      fetchcnt: "0"
      fetchtime: "00:00:00.000000"
      holdstatus: "NOTAPPLIC"
      installagent: "CSDAPI"
      installtime: "2020-12-15T02:34:33.000000+00:00"
      installusrid: "YQCHEN"
      jvmclass: ""
      jvmserver: ""
      language: "NOTDEFINED"
      length: "0"
      library: ""
      librarydsn: ""
      loadpoint: "FF000000"
      lpastat: "NOTAPPLIC"
      newcopycnt: "0"
      operation: ""
      pgrjusecount: "0"
      platform: ""
      program: "ANSITEST"
      progtype: "PROGRAM"
      remotename: ""
      remotesystem: ""
      removecnt: "0"
      rescount: "0"
      residency: "NONRESIDENT"
      rloading: "0.000"
      rplid: "0"
      rremoval: "0.000"
      runtime: "UNKNOWN"
      ruse: "0.000"
      sharestatus: "PRIVATE"
      status: "DISABLED"
      transid: ""
      useagelstat: "0"
      usecount: "0"
      usefetch: "0.000"
request:
  description: Information about the request that was made to CMCI.
  returned: success
  type: dict
  contains:
    body:
      description: The XML body sent with the request, if any.
      returned: success
      type: str
    method:
      description: The HTTP method used for the request.
      returned: success
      type: str
    url:
      description: The URL used for the request.
      returned: success
      type: str
feedback:
  description: Diagnostic data from FEEDBACK records associated with the request
  returned: cmci error
  type: list
  elements: dict
  contains:
    action:
      description: The name of the action that has failed.
      returned: cmci error
      type: str
    attribute1:
      description: The name of one of up to six attributes associated with the error.
      returned: cmci error
      type: str
    attribute2:
      description: The name of one of up to six attributes associated with the error.
      returned: cmci error
      type: str
    attribute3:
      description: The name of one of up to six attributes associated with the error.
      returned: cmci error
      type: str
    attribute4:
      description: The name of one of up to six attributes associated with the error.
      returned: cmci error
      type: str
    attribute5:
      description: The name of one of up to six attributes associated with the error.
      returned: cmci error
      type: str
    attribute6:
      description: The name of one of up to six attributes associated with the error.
      returned: cmci error
      type: str
    eibfn:
      description: The function code associated with the request.
      returned: cmci error
      type: str
    eibfn_alt:
      description: The name of the function associated with the request.
      returned: cmci error
      type: str
    errorcode:
      description: The CICSPlex® SM error code associated with the resource.
      returned: cmci error
      type: str
    eyu_cicsname:
      description: The name of the CICS region or CICSplex associated with the error.
      returned: cmci error
      type: str
    keydata:
      description: A string of data that identifies the instance of a resource associated with the error.
      returned: cmci error
      type: str
    resp:
      description: The CICS RESP code or the CICSPlex SM API EYUDA response code as a numeric value.
      returned: cmci error
      type: str
    resp2:
      description: The CICS RESP2 code or the CICSPlex SM API EYUDA reason code as a numeric value.
      returned: cmci error
      type: str
    resp_alt:
      description: >
        The text equivalent for the resp value. For example, the text equivalent of a resp value of 16 is INVREQ.
      returned: cmci error
      type: str
    installerror:
      description: >
        Contains diagnostic data from a BINSTERR record associated with a CICS® management client interface PUT install request.
      returned: cmci error
      type: list
      elements: dict
      contains:
        eibfn:
          description: The function code associated with the request.
          returned: cmci error
          type: str
        eyu_cicsname:
          description: The name of the CICS region or CICSplex associated with the installation error.
          returned: cmci error
          type: str
        cresp1:
          description: The CICS RESP code or the CICSPlex® SM API EYUDA response code as a numeric value.
          returned: cmci error
          type: str
        cresp2:
          description: The CICS RESP2 code or the CICSPlex SM API EYUDA reason code as a numeric value.
          returned: cmci error
          type: str
        errorcode:
          description: The CICSPlex SM error code associated with the resource.
          returned: cmci error
          type: str
        ressname:
          description: The name of the resource associated with the error.
          returned: cmci error
          type: str
        resver:
          description: The version number of the resource associated with the error.
          returned: cmci error
          type: str
    inconsistentscope:
      description: >
        Contains diagnostic data from a BINCONSC record associated with a CICS® management client interface PUT request.
      returned: cmci error
      type: list
      elements: dict
      contains:
        eibfn:
          description: The function code associated with the request.
          returned: cmci error
          type: str
        eyu_cicsname:
          description: The name of the CICS region or CICSplex associated with the installation error.
          returned: cmci error
          type: str
        erroroperation:
          description: A numeric value that identifies the operation being performed when the error occurred.
          returned: cmci error
          type: str
        errorcode:
          description: The CICSPlex® SM error code associated with the resource.
          returned: cmci error
          type: str
        targetassignment:
          description: The assignment for the target scope.
          returned: cmci error
          type: str
        targetdescription:
          description: The resource description for the target scope.
          returned: cmci error
          type: str
        relatedassignment:
          description: The resource assignment for the related scope.
          returned: cmci error
          type: str
        relateddescription:
          description: The resource description for the related scope.
          returned: cmci error
          type: str
        relatedscope:
          description: The name of the related scope.
          returned: cmci error
          type: str
    inconsistentset:
      description: >
        Contains diagnostic data from a BINCONRS record associated with a CICS® management client interface PUT request.
      returned: cmci error
      type: list
      elements: dict
      contains:
        candidatename:
          description: The name of the candidate resource.
          returned: cmci error
          type: str
        candidateversion:
          description: The version number of the candidate resource.
          returned: cmci error
          type: str
        candidategroup:
          description: The resource group of the candidate resource.
          returned: cmci error
          type: str
        candidateassignment:
          description: The assignment of the candidate resource.
          returned: cmci error
          type: str
        candidatedescription:
          description: The description of the candidate resource.
          returned: cmci error
          type: str
        candidateusage:
          description: The assignment usage of the candidate resource.
          returned: cmci error
          type: str
        candidatesystemgroup:
          description: The system group of the candidate resource.
          returned: cmci error
          type: str
        candidatetype:
          description: The system type of the candidate resource.
          returned: cmci error
          type: str
        candidateoverride:
          description: The assignment override of the candidate resource.
          returned: cmci error
          type: str
        eyu_cicsname:
          description: The name of the CICS region associated with the installation error.
          returned: cmci error
          type: str
        erroroperation:
          description: >
            A numeric value that identifies that the operation being performed when the error occurred
          returned: cmci error
          type: str
        existingname:
          description: The name of the existing resource.
          returned: cmci error
          type: str
        existingversion:
          description: The version number of the existing resource.
          returned: cmci error
          type: str
        existinggroup:
          description: The resource group of the existing resource.
          returned: cmci error
          type: str
        existingassignment:
          description: The assignment of the existing resource.
          returned: cmci error
          type: str
        existingdescription:
          description: The description of the existing resource.
          returned: cmci error
          type: str
        existingusage:
          description: The assignment usage of the existing resource.
          returned: cmci error
          type: str
        existingsystemgroup:
          description: The system group of the existing resource.
          returned: cmci error
          type: str
        existingtype:
          description: The system type of the existing resource.
          returned: cmci error
          type: str
        existingoverride:
          description: The assignment override of the existing resource.
          returned: cmci error
          type: str
"""

from ansible_collections.ibm.ibm_zos_cics.plugins.module_utils.cmci import (
    AnsibleCMCIModule, RESOURCES_ARGUMENT
)

from typing import Dict, Optional

_RECORD_COUNT = 'record_count'
_FAIL_ON_NODATA = 'fail_on_nodata'


class AnsibleCMCIGetModule(AnsibleCMCIModule):
    def __init__(self):
        super(AnsibleCMCIGetModule, self).__init__('GET')

    def init_argument_spec(self):  # type: () -> Dict
        argument_spec = super(AnsibleCMCIGetModule, self).init_argument_spec()
        argument_spec.update({
            _RECORD_COUNT: {
                'type': 'int'
            },
            _FAIL_ON_NODATA: {
                'type': 'bool',
                'default': True
            }
        })
        argument_spec.update(RESOURCES_ARGUMENT)
        return argument_spec

    def init_request_params(self):  # type: () -> Optional[Dict[str, str]]
        return self.get_resources_request_params()

    def init_url(self):  # type: () -> str
        url = super(AnsibleCMCIGetModule, self).init_url()

        if self._p.get(_RECORD_COUNT):
            url = url + '//' + str(self._p.get(_RECORD_COUNT))

        return url

    def get_ok_cpsm_response_codes(self):
        ok_codes = super(AnsibleCMCIGetModule, self).get_ok_cpsm_response_codes()

        if not self._p.get(_FAIL_ON_NODATA):
            ok_codes.append(1027)

        return ok_codes


def main():
    AnsibleCMCIGetModule().main()


if __name__ == '__main__':
    main()
