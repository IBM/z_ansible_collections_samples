# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
import pytest
from pprint import pprint

from ibm_zos_ims.tests.functional.module_utils.ims_test_catalog_utils import CatalogInputParameters as cp  # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_catalog_utils import load_catalog, purge_catalog  # pylint: disable=import-error

__metaclass__ = type

"""
Scenario 4: Invalid mode option=load, managed_acbs with stage
"""


def test_catalog_stage_managed_acbs_in_update(ansible_zos_module):
    hosts = ansible_zos_module
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.LOADMODE,
                 validation_msg="You cannot update or stage ACBs in catalog LOAD mode.",
                 rc=1,
                 changed=False,
                 control_statements={
                     'managed_acbs': {
                         'stage': {
                             'save_acb': "UNCOND",
                             'clean_staging_dataset': True
                         }
                     }
                 })


"""
Scenario 5: Tests a catalog update mode while specifying managedacbs update or stage.
It then tries to specify the bootstrap dataset, or the directory datasets, or the directory
staging datasets. The ansible module should throw an error as this is not allowed. (test
with disposition share, this may be allowed).
"""


def test_catalog_update_mode_boostrap_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    bootstrap_data_set = {
        'dataset_name': cp.BSDS,
        'disposition': 'SHR',
        'normal_disposition': 'CATLG',
        'primary': 350,
        'volumes': ['222222']
    }
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 bootstrap_dataset=bootstrap_data_set,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.UPDATEMODE,
                 validation_msg="You cannot define directory datasets, the bootstrap dataset, "
                                "or directory staging datasets with MANAGEDACBS=STAGE or MANAGEDACBS=UPDATE",
                 rc=1,
                 changed=False,
                 control_statements={
                     'managed_acbs': {
                         'stage': {
                             'save_acb': "UNCOND",
                             'clean_staging_dataset': True
                         }
                     }
                 })


def test_catalog_update_mode_directory_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    directory_data_sets = [
        {
            'dataset_name': cp.DIR1,
            'disposition': 'NEW',
            'normal_disposition': 'CATLG',
            'primary': 200,
            'volumes': ['222222']
        },
        {
            'dataset_name': cp.DIR2,
            'disposition': 'NEW',
            'normal_disposition': 'CATLG',
            'primary': 200,
            'volumes': ['222222']
        },
    ]
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 directory_datasets=directory_data_sets,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.UPDATEMODE,
                 validation_msg="You cannot define directory datasets, the bootstrap dataset, "
                                "or directory staging datasets with MANAGEDACBS=STAGE or MANAGEDACBS=UPDATE",
                 rc=1,
                 changed=False,
                 control_statements={
                     'managed_acbs': {
                         'stage': {
                             'save_acb': "UNCOND",
                             'clean_staging_dataset': True
                         }
                     }
                 })


def test_catalog_update_mode_directory_staging_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    directory_staging_data_set = {
        'dataset_name': cp.STAGE,
        'disposition': 'NEW',
        'normal_disposition': 'CATLG',
        'primary': 300,
        'volumes': ['222222']
    }
    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 directory_staging_dataset=directory_staging_data_set,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.UPDATEMODE,
                 validation_msg="You cannot define directory datasets, the bootstrap dataset, "
                                "or directory staging datasets with MANAGEDACBS=STAGE or MANAGEDACBS=UPDATE",
                 rc=1,
                 changed=False,
                 control_statements={
                     'managed_acbs': {
                         'stage': {
                             'save_acb': "UNCOND",
                             'clean_staging_dataset': True
                         }
                     }
                 })
