/* rexx  __ANSIBLE_ENCODE_EBCDIC__  */
/* WANT_JSON */

/* Copyright (c) IBM Corporation 2019, 2020 */
/* Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0) */

/*
ANSIBLE_METADATA = {'metadata_version': '1.1',
                    'status': ['preview'],
                    'supported_by': 'community'}

DOCUMENTATION = '''
---
module: zos_ping
version_added: 2.9
short_description: Ping z/OS and check dependencies.
description:
  - M(zos_ping) verifies the presence of z/OS Web Client Enablement Toolkit,
    iconv, and Python.
  - M(zos_ping) returns C(pong) when the target host is not missing any required dependencies.
  - If the target host is missing optional dependencies, the M(zos_ping) will return one or more warning messages.
  - If a required dependency is missing from the target host, an explanatory message will be returned with the module failure.
  - The `zos_ssh` connection plugin must be used for this module to function correctly.
author:
  - "Vijay Katoch"
  - "Blake Becker (@blakeinate)"
'''

EXAMPLES = '''
- name: Ping the z/OS host and perform resource checks
  zos_ping:
  register: result
'''

RETURN = '''
ping:
  description: Should contain the value "pong" on success.
  type: str
warnings:
  description: List of warnings returned from stderr when performing resource checks.
  type: list
  elements: str
'''
*/

Parse Arg argFile .

pythonName       = 'Python'
majVersionPython = 3
minVersionPython = 8
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

/* Check for Python version >= 3.8 eg: 'Python 3.8.2' */
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
