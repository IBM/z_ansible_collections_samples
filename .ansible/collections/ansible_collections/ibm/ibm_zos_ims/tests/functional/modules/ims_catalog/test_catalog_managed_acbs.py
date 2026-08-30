# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division, print_function)
import pytest
import re
from math import ceil
from pprint import pprint
from ibm_zos_ims.tests.functional.module_utils.ims_test_catalog_utils import CatalogInputParameters as cp   # pylint: disable=import-error
from ibm_zos_ims.tests.functional.module_utils.ims_test_catalog_utils import load_catalog, purge_catalog   # pylint: disable=import-error

__metaclass__ = type

BYTES_PER_TRK = 56664
BYTES_PER_CYL = BYTES_PER_TRK * 15
BYTES_PER_KB = 1024
BYTES_PER_MB = 1048576


# Scenario 2: Load mode, managed_acbs - setup=True
def test_catalog_load_managed_acbs(ansible_zos_module):

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
                 validation_msg="DFS4533I",
                 control_statements={'managed_acbs': {"setup": True}})

    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)


# Scenario 3: Update mode, managed_acbs - stage options(save_acb=UNCOND and clean_staging_dataset=True)
# and update option(replace_acb=UNCOND)
def test_catalog_update_managed_acbs_stage_and_update(ansible_zos_module):
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
                 mode=cp.UPDATEMODE,
                 validation_msg="DFS4536I",
                 control_statements={
                     'managed_acbs': {
                         'stage': {
                             'save_acb': "UNCOND",
                             'clean_staging_dataset': True
                         }
                     }
                 })

    load_catalog(hosts,
                 psb_lib=cp.PSBLIB,
                 dbd_lib=cp.DBDLIB,
                 acb_lib=cp.ACBLIB,
                 steplib=cp.STEPLIB,
                 reslib=cp.RESLIB,
                 proclib=cp.PROCLIB,
                 primary_log_dataset=cp.PRIMARYLOG,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.UPDATEMODE,
                 validation_msg="DFS4534I",
                 control_statements={'managed_acbs': {'update': {'replace_acb': "UNCOND"}}})

    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)


# Setup the Catalog while defining the bootstrap dataset.
def test_catalog_define_bootstrap(ansible_zos_module):
    hosts = ansible_zos_module

    # Delete the bootstrap dataset first
    response = hosts.all.zos_data_set(name=cp.BSDS, state="absent")
    for result in response.contacted.values():
        assert result['message'] == ''
        if result['changed'] is False:
            response = hosts.all.zos_data_set(name=cp.BSDS, state="absent", volume="SCR03")

    # Load catalog while defining the bootstrap dataset
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
                 validation_msg="DFS4533I",
                 bootstrap_dataset={
                     'dataset_name': cp.BSDS,
                     'disposition': 'NEW',
                     'normal_disposition': 'CATLG',
                     'primary': 350,
                     'volumes': ['222222']
                 },
                 control_statements={'managed_acbs': {"setup": True}})

    # Verify the bootstrap dataset was created with the specified parameters
    estimated_size_in_bytes = 0
    response = hosts.all.command("dls -s " + cp.BSDS)

    for result in response.contacted.values():
        for line in result.get("stdout_lines", []):
            lineList = line.split()
            estimated_size_in_bytes = int(lineList[-1])
        estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, "TRK")
        assert estimated_size_in_unit == 350

    # Purge the catalog
    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)

    # Finally delete the boostrap dataset again
    response = hosts.all.zos_data_set(name=cp.BSDS, state="absent")
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['message'] == ''


# Setup the Catalog while defining the staging dataset.
def test_catalog_define_staging(ansible_zos_module):
    hosts = ansible_zos_module

    # Delete the staging dataset first
    response = hosts.all.zos_data_set(name=cp.STAGE, state="absent")
    for result in response.contacted.values():
        assert result['message'] == ''
        if result['changed'] is False:
            response = hosts.all.zos_data_set(name=cp.STAGE, state="absent", volume="SCR03")

    # Load catalog while defining the staging dataset
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
                 validation_msg="DFS4533I",
                 directory_staging_dataset={
                     'dataset_name': cp.STAGE,
                     'disposition': 'NEW',
                     'normal_disposition': 'CATLG',
                     'primary': 300,
                     'volumes': ['222222']
                 },
                 control_statements={'managed_acbs': {"setup": True}})

    # Verify the staging dataset was created with the specified parameters
    estimated_size_in_bytes = 0
    response = hosts.all.command("dls -s " + cp.STAGE)

    for result in response.contacted.values():
        for line in result.get("stdout_lines", []):
            pprint("dls stdout: " + line)
            lineList = line.split()
            estimated_size_in_bytes = int(lineList[-1])
        estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, "TRK")
        assert estimated_size_in_unit == 300

    # Purge the catalog
    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)

    # Finally delete the staging dataset again
    response = hosts.all.zos_data_set(name=cp.STAGE, state="absent")
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['message'] == ''


# Setup the Catalog while defining the directory datasets.
def test_catalog_define_directory(ansible_zos_module):
    hosts = ansible_zos_module

    # Delete the directory datasets first
    response = hosts.all.zos_data_set(batch=cp.DIR_BATCH)
    for result in response.contacted.values():
        assert result['message'] == ''
        if result['changed'] is False:
            response = hosts.all.zos_data_set(name=cp.DIR_BATCH, state="absent", volume="SCR03")

    # Load catalog while defining the directory datasets
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
                 validation_msg="DFS4533I",
                 directory_datasets=[
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

                 ],
                 control_statements={'managed_acbs': {"setup": True}})

    # Verify the directory datasets were created with the specified parameters
    estimated_size_in_bytes = 0
    response = hosts.all.command("dls -s " + cp.DIR1)
    for result in response.contacted.values():
        for line in result.get("stdout_lines", []):
            lineList = line.split()
            estimated_size_in_bytes = int(lineList[-1])
        estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, "TRK")
        assert estimated_size_in_unit == 200

    response = hosts.all.command("dls -s " + cp.DIR2)
    for result in response.contacted.values():
        for line in result.get("stdout_lines", []):
            lineList = line.split()
            estimated_size_in_bytes = int(lineList[-1])
        estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, "TRK")
        assert estimated_size_in_unit == 200

    # Purge the catalog
    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)

    # Finally delete the directory datasets again
    response = hosts.all.zos_data_set(batch=cp.DIR_BATCH)
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['message'] == ''


"""
Scenario 7: Test the creation of the temp_acb_dataset, which holds ACBs that reference
GSAM database. Test catalog in load mode with managed acbs setup = true or no managedacbs
options specified. Specify the temp_acb_dataset fields. The temp_acb_dataset can be named
anything, I recommend sticking with your first two IMS library qualifiers with the 3rd
qualifier being whatever you want. Verify the temp acb dataset is created with the specified
values. Purge the catalog.
"""


def test_creation_of_temp_acb_dataset_with_managed_acbs(ansible_zos_module):
    hosts = ansible_zos_module

    # Delete TEMP_ACB data set before the test
    response = hosts.all.zos_data_set(name=cp.TEMP_ACB, state="absent")
    for result in response.contacted.values():
        assert result['message'] == ''

    temp_acb_data_set = {
        'dataset_name': cp.TEMP_ACB,
        'disposition': 'NEW',
        'normal_disposition': 'CATLG',
        'primary': 200,
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
                 temp_acb_dataset=temp_acb_data_set,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.LOADMODE,
                 validation_msg="DFS4533I",
                 control_statements={
                     'managed_acbs': {
                         'setup': True
                     }
                 })

    estimated_size_in_bytes = 0
    response = hosts.all.command("dls -s " + cp.TEMP_ACB)
    for result in response.contacted.values():
        for line in result.get("stdout_lines", []):
            lineList = line.split()
            estimated_size_in_bytes = int(lineList[-1])
        estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, "TRK")
        assert estimated_size_in_unit == 200

    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)

    # Delete TEMP_ACB data set after the test
    response = hosts.all.zos_data_set(name=cp.TEMP_ACB, state="absent")
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['message'] == ''


def test_creation_of_temp_acb_dataset_without_managed_acbs(ansible_zos_module):
    hosts = ansible_zos_module

    # Delete TEMP_ACB data set before the test
    response = hosts.all.zos_data_set(name=cp.TEMP_ACB, state="absent")
    for result in response.contacted.values():
        assert result['message'] == ''

    temp_acb_data_set = {
        'dataset_name': cp.TEMP_ACB,
        'disposition': 'NEW',
        'normal_disposition': 'CATLG',
        'primary': 200,
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
                 temp_acb_dataset=temp_acb_data_set,
                 buffer_pool_param_dataset=cp.BUFFERPOOL,
                 mode=cp.LOADMODE,
                 validation_msg="DFS4434I"
                 )

    estimated_size_in_bytes = 0
    response = hosts.all.command("dls -s " + cp.TEMP_ACB)
    for result in response.contacted.values():
        for line in result.get("stdout_lines", []):
            lineList = line.split()
            estimated_size_in_bytes = int(lineList[-1])
        estimated_size_in_unit = bytes_to_unit(estimated_size_in_bytes, "TRK")
        assert estimated_size_in_unit == 200

    purge_catalog(hosts,
                  psb_lib=cp.PSBLIB,
                  dbd_lib=cp.DBDLIB,
                  steplib=cp.STEPLIB,
                  reslib=cp.RESLIB,
                  proclib=cp.PROCLIB,
                  primary_log_dataset=cp.PRIMARYLOG,
                  buffer_pool_param_dataset=cp.BUFFERPOOL,
                  mode=cp.PURGEMODE,
                  validation_msg="",
                  delete=cp.DELETES,
                  managed_acbs=True)

    # Delete TEMP_ACB data set after the test
    response = hosts.all.zos_data_set(name=cp.TEMP_ACB, state="absent")
    for result in response.contacted.values():
        assert result['changed'] is True
        assert result['message'] == ''


def bytes_to_unit(number_of_bytes, unit):
    size = 0
    unit = unit.lower()
    if number_of_bytes == 0:
        number_of_bytes = 1
    if unit == "cyl":
        size = byte_to_cyl(number_of_bytes)
    elif unit == "kb" or unit == "k":
        size = byte_to_kilobyte(number_of_bytes)
    elif unit == "mb" or unit == "m":
        size = byte_to_megabyte(number_of_bytes)
    else:
        size = byte_to_trk(number_of_bytes)
    return size


def byte_to_trk(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_TRK)


def byte_to_cyl(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_CYL)


def byte_to_kilobyte(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_KB)


def byte_to_megabyte(number_of_bytes):
    return ceil(number_of_bytes / BYTES_PER_MB)
