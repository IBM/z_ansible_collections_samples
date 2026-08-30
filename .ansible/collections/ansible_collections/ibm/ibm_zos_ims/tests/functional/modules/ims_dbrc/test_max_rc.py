# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_dbrc_utils import DBRCInputParameters as ip  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error

__metaclass__ = type


def test_delete_db_that_does_not_exist_without_max_rc(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["DELETE.DB DBD(DOESNOTEXIST2)"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.FAILURE_MSG


def test_delete_db_that_does_not_exist(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["DELETE.DB DBD(DOESNOTEXIST)"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3, max_rc="12"
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.SUCCESS_MSG


def test_max_rc_not_high_enough(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["DELETE.DB DBD(DOESNOTEXIST)"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3, max_rc="4"
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.FAILURE_MSG


def test_delete_db_that_does_not_exist_different_max_rc(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.ims_dbrc(
        command=["DELETE.DB DBD(DOESNOTEXIST2)"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3, max_rc=12
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['msg'] == em.SUCCESS_MSG
