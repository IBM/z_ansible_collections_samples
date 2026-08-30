# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_dbrc_utils import DBRCInputParameters as ip  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error

__metaclass__ = type


def test_single_change_command_no_parameters(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["CHANGE.RECON"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.SUCCESS_MSG
        assert result['dbrc_output'][0]['COMMAND'] == 'CHANGE.RECON'
        assert result['dbrc_output'][0]['MESSAGES']
        assert result['dbrc_output'][0]['OUTPUT']


def test_single_change_command_incorrect_parameters(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["CHANGE.PRILOG NORMAL"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.FAILURE_MSG


def test_single_change_command(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["CHANGE.PRILOG"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.FAILURE_MSG
