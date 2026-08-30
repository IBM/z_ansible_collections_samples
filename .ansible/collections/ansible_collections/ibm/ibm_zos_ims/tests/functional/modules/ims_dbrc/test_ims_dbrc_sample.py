# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
from pprint import pprint
import pytest
from ibm_zos_ims.tests.functional.module_utils.ims_test_dbrc_utils import DBRCInputParameters as ip  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_ims.plugins.module_utils.ims_module_error_messages import DBRCErrorMessages as em  # pylint: disable=import-error

__metaclass__ = type


def test_ims_dbrc_sample(ansible_zos_module):
    hosts = ansible_zos_module
    # Create the JCLOUT data set if it's not already present
    response = hosts.all.zos_data_set(name=ip.GENJCL_OUTPUT_DS, state="present", type="SEQ", replace=True)
    for ds_result in response.contacted.values():
        assert ds_result['changed']

    results = hosts.all.ims_dbrc(
        command=[
            "LIST.RECON STATUS",
            "LIST.DB ALL",
            "LIST.BKOUT ALL",
            "LIST.LOG",
            "LIST.CAGRP"],
        steplib=ip.STEPLIB, dbd_lib=ip.DBD_LIB, genjcl_input_dataset=ip.GENJCL_INPUT_DS,
        genjcl_output_dataset=ip.GENJCL_OUTPUT_DS, recon1=ip.RECON1, recon2=ip.RECON2, recon3=ip.RECON3
    )
    for result in results.contacted.values():
        pprint(result)
        assert result['changed']
        assert result['msg'] == em.SUCCESS_MSG
