# Copyright (c) IBM Corporation 2023, 2024
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division, print_function
__metaclass__ = type

# from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
#     MissingZOAUImport,
#     MissingImport,
# )

from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.dd_statement import (  # pylint: disable=import-error
    DDStatement,
    StdoutDefinition,
    StdinDefinition,
)


def get_init_command(module, result, args):
    """Get init command.

    Parameters
    ----------
    module : obj
        Object from the collection.
    result : dic
        Results dictionary.
    args : dict
        Arguments to be formatted.

    Returns
    -------
    str
        Formatted JCL strings for zos_mvs_raw.
    """

    # Get parameters from playbooks
    address = args.get('address')
    verify_volid = args.get('verify_volid')
    verify_offline = args.get('verify_offline')
    volid = args.get('volid')
    vtoc_size = args.get('vtoc_size')
    index = args.get('index')
    verify_volume_empty = args.get('verify_volume_empty')
    sms_managed = args.get('sms_managed')

    # Let AnsibleModule param parsing handle this check.
    # validate parameters
    # if address is None:
    #     msg = 'Volume address must be defined'
    #     # raise Exception(msg)
    #     module.fail_json(msg) # TODO - fail with result -- do i want an init class so i can self.fail_json?

    # let ICKDSF handle this check. expect RC=12
    # try:
    #     int(address, 16)
    # except ValueError:
    #     result['failed'] = True
    #     msg = 'address must be 3 or 4 64-bit hexadecimal digits'
    #     # raise Exception(msg)
    #     module.fail_json(msg, **result) # TODO - fail with result -- do i want an init class so i can self.fail_json?

    # convert playbook args to JCL parameters
    cmd_args = {
        'address': 'unit({0})'.format(address)
    }

    if vtoc_size:
        cmd_args['vtoc_size'] = 'vtoc(0, 1, {0})'.format(vtoc_size)
    else:
        cmd_args['vtoc_size'] = ''
    if volid:
        cmd_args['volid'] = 'volid({0})'.format(volid)
    else:
        cmd_args['volid'] = ''
    if not verify_volid:
        cmd_args['verify_volid'] = 'noverify'
    else:
        cmd_args['verify_volid'] = 'verify({0})'.format(verify_volid)
    if verify_offline:
        cmd_args['verify_offline'] = 'verifyoffline'
    else:
        cmd_args['verify_offline'] = 'noverifyoffline'
    if verify_volume_empty:
        cmd_args['verify_volume_empty'] = 'nods'
    else:
        cmd_args['verify_volume_empty'] = 'ds'
    if index:
        cmd_args['index'] = ''
    else:
        cmd_args['index'] = 'noindex'
    if sms_managed:
        cmd_args['sms_managed'] = 'storagegroup'
    else:
        cmd_args['sms_managed'] = ''

    # Format into JCL strings for zos_mvs_raw
    cmd = [
        ' init {0} {1} {2} {3} - '.format(
            cmd_args['address'],
            cmd_args['verify_volid'],
            cmd_args['verify_offline'],
            cmd_args['volid']),
        ' {0} {1} {2} {3}'.format(
            cmd_args['vtoc_size'],
            cmd_args['sms_managed'],
            cmd_args['verify_volume_empty'],
            cmd_args['index'])]

    return cmd


def init(module, result, parsed_args):
    """Init

    Parameters
    ----------
    module : object
        The module to give results of.
    result : dict
        The results of the process.
    parsed_args : dict
        Parsed arguments to be converted to command.

    Returns
    -------
    dict
        The dictionary with the results.
    """
    # Convert args parsed from module to ickdsf INIT command
    cmd = get_init_command(module, result, parsed_args)

    # TODO - add error handling here and in get_init_command() for "bad" cmd

    # define/build DDs to pass into MVS Command

    if parsed_args.get('tmp_hlq'):
        sysInDDStatement = DDStatement("SYSIN", StdinDefinition(cmd, tmphlq=parsed_args.get('tmp_hlq')))
    else:
        sysInDDStatement = DDStatement("SYSIN", StdinDefinition(cmd))

    # tmphlq is not currently captured in the construction of the StdoutDefinition DD.
    # tmphlq is handled in the mvscmd.execute_authorized call in this case.
    sysprintDDStatement = DDStatement("SYSPRINT", StdoutDefinition())

    dds = []
    dds.append(sysprintDDStatement)
    dds.append(sysInDDStatement)

    # invoke MVS Command
    if parsed_args.get('tmp_hlq'):
        response = MVSCmd.execute_authorized("ICKDSF", dds, parm='NOREPLYU,FORCE', tmp_hlq=parsed_args.get('tmp_hlq'))
        # uncomment the following line to see MVSCmd verbose output in stderr.
        # response = MVSCmd.execute_authorized("ICKDSF", dds, parm='NOREPLYU,FORCE', verbose=True, tmp_hlq=parsed_args.get('tmp_hlq'))
    else:
        response = MVSCmd.execute_authorized("ICKDSF", dds, parm='NOREPLYU,FORCE')
        # uncomment the following line to see MVSCmd verbose output in stderr.
        # response = MVSCmd.execute_authorized("ICKDSF", dds, parm='NOREPLYU,FORCE', verbose=True)

    rc = response.rc

    result['rc'] = rc
    result['content'] = response.stdout.strip().split("\n")
    if response.stderr:
        result['stderr'] = response.stderr

    if rc != 0:
        result['failed'] = True
        msg = "Non-zero return code. See 'content' for details."
        module.fail_json(msg=msg, **result)
    else:
        result['changed'] = True

    return dict(result)
