# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_dbrc_utils import DBRCInputParameters as ip  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error

__metaclass__ = type


def test_invalid_max_rc(ansible_zos_module):
    hosts = ansible_zos_module
    invalid_max_rc = [12.43, "Basketball", "$@#$%^", "20twenty2"]
    for invalid_rc in invalid_max_rc:
        results = hosts.all.ims_dbrc(
            command=["LIST.RECON STATUS"],
            steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
            recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3, max_rc=invalid_rc
        )
        for result in results.contacted.values():
            pprint(result)
            assert result['msg'] != em.SUCCESS_MSG
            assert result['changed'] is False
