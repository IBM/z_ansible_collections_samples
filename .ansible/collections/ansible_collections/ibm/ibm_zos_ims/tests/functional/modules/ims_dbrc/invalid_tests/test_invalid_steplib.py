# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_dbrc_utils import DBRCInputParameters as ip  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error

__metaclass__ = type


def test_missing_steplib(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["LIST.RECON STATUS"],
        dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['changed'] is False
        assert result['msg'] == em.MISSING_STEPLIB


def test_single_invalid_steplib(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["LIST.RECON STATUS"],
        steplib=["IMSTESTL.INVALID.STEPLIB"], dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    # error_message = "Unable to load program DSPURX00"
    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['msg'] == em.SUCCESS_MSG


def test_invalid_steplib_with_valid_steplib(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["LIST.RECON STATUS"],
        steplib=ip.STEPLIB + ["IMSTESTL.INVALID.STEPLIB"], dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['msg'] == em.SUCCESS_MSG


"""
Author: An Lam
"""


def test_missing_steplib2(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command="LIST.DB ALL",
        dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.MISSING_STEPLIB


def test_missing_all_recon(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command="LIST.RECON STATUS",
        steplib=ip.STEPLIB,
        dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS
    )
    for result in results.contacted.values():
        pprint(result)
        print("+++ result[msg] = ", result['msg'])
        assert result['msg'].find(em.DYNALLOC_RECON_REQUIREMENT_MSG) != -1


def test_missing_all(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command="",
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.MISSING_STEPLIB

# def test_dup_params(ansible_zos_module):
#     hosts = ansible_zos_module
#     results = hosts.all.ims_dbrc(
#         command="LIST.RECON STATUS",
#         dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
#         recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3,
#         command="LIST.RECON STATUS",
#         dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
#         recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
#     )
#     for result in results.contacted.values():
#         pprint(result)
#         assert result['msg'] == em.MISSING_STEPLIB


def test_single_invalid_steplib2(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command="LIST.DB ALL",
        steplib=["IMSTESTL.INVALID.STEPLIB"], dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['msg'] == em.SUCCESS_MSG
