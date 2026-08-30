/* rexx  __ANSIBLE_ENCODE_EBCDIC__  */
/* WANT_JSON */

/* Copyright (c) IBM Corporation 2019, 2023 */

/* Licensed under the Apache License, Version 2.0 (the "License"); */
/* you may not use this file except in compliance with the License. */
/* You may obtain a copy of the License at */
/*     http://www.apache.org/licenses/LICENSE-2.0 */
/* Unless required by applicable law or agreed to in writing, software */
/* distributed under the License is distributed on an "AS IS" BASIS, */
/* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. */
/* See the License for the specific language governing permissions and */
/* limitations under the License. */

/*


DOCUMENTATION = '''
---
module: zos_ping
version_added: 1.1.0
short_description: Ping z/OS and check dependencies.
description:
  - L(zos_ping,./zos_ping.html) verifies the presence of z/OS Web Client Enablement Toolkit,
    iconv, and Python.
  - L(zos_ping,./zos_ping.html) returns C(pong) when the target host is not missing any required
    dependencies.
  - If the target host is missing optional dependencies, the L(zos_ping,./zos_ping.html) will
    return one or more warning messages.
  - If a required dependency is missing from the target host, an explanatory
    message will be returned with the module failure.
author:
  - "Vijay Katoch (@vijayka)"
  - "Blake Becker (@blakeinate)"
  - "Demetrios Dimatos (@ddimatos)"
options: {}
'''

EXAMPLES = '''
- name: Ping the z/OS host and perform resource checks
  zos_ping:
  register: result
'''

RETURN = '''
ping:
  description: Should contain the value "pong" on success.
  returned: always
  type: str
  sample: pong
warnings:
  description: List of warnings returned from stderr when performing resource
  checks.
  returned: failure
  type: list
  elements: str
'''
*/

Parse Arg argFile .

pythonName       = 'Python'
majVersionPython = 3
minVersionPython = 10
warningJsonList = ''

If (argFile = '') Then Do
    failModule('Internal Error: JSON argument file missing' || ESC_N, "", 16)
End
newArgFile1 = argFile || 1
/* Check for iconv utility by converting the JSON argument file form ASCII to EBCDIC */
retC = bpxwunix('iconv -f ISO8859-1 -t IBM-1047 ' argFile,,stderr.)

If (retC <> 0) Then Do
    failModule('Command iconv not found.', stderr, retC)
End

/* Load z/OS Web Client Enablement Toolkit to verify system has z/OS Web Client Enablement toolkit installed */ Call hwtcalls "on"
Address hwtjson 'hwtConst returnCode resbuf.'
If (rc <> 0 | returnCode <> HWTJ_OK) Then Do
    retC = rc
    errmsg = 'Error: Unable to start JSON Parser may be due to missing',
             'z/OS Web Client enablement toolkit.' || ESC_N
    failModule(errmsg, "", retC)
End

/* Check for Python version >= 3.10 eg: 'Python 3.10.0' */
retC = bpxwunix('python3 --version', out., err.)
If (err.0 > 0) Then Do
    Do index=1 To err.0
        warningJsonList = addWarningToList(warningJsonList, 'Python Warning: ' || err.index)
    End
End
Else Do
    If (out.0 > 0) Then Do
        parse Var out.1 name version
        parse Var version majVer '.' minVer '.' micVer
        If (pythonName == strip(name)) Then Do
            If (majVer < majVersionPython | minVer < minVersionPython) Then Do
                warningJsonList = addWarningToList(warningJsonList, "Python Warning: Python not up to the level to support z/OS modules")
            End
        End
        Else Do
            warningJsonList = addWarningToList(warningJsonList, "Python Warning: Incorrect Python Found")
        End
    End
End

retJson = '{"changed":false,"ping": "pong","failed":false'
/* Construct a JSON list for warnings */
If (warningJsonList <> '') Then
    retJson = retJson || ',"warnings": [' || warningJsonList  || ']'
retJson = retJson || '}'

Say retJson
Exit(0)


/* Build a comma separated list of warnings */
addWarningToList:
PARSE ARG warningList, warning
if (warningList <> '') Then
warningList = warningist || ','
warningList = warningList || '"' || warning || '"'
return warningList


/* Fail the module when unacceptable situation has occurred */
failModule:
PARSE ARG output,stderr,retC
Address Syscall
'Write' 2 'output' length(output)
Do index=1 To stderr.0
    errmsg = stderr.index || ESC_N
    'Write' 2 'errmsg' length(errmsg)
End
Exit(retC)
return
