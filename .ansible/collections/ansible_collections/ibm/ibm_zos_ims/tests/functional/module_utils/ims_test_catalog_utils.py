from __future__ import (absolute_import, division, print_function)
import pytest
from pprint import pprint

__metaclass__ = type


def load_catalog(hosts, validation_msg, mode, psb_lib, dbd_lib, steplib, reslib, proclib, primary_log_dataset,
                 buffer_pool_param_dataset, acb_lib, modstat=None, online_batch=None, dbrc=None, ims_id=None, irlm_id=None,
                 control_statements=None, bootstrap_dataset=None,
                 directory_datasets=None, temp_acb_dataset=None, directory_staging_dataset=None,
                 secondary_log_dataset=None, sysabend=None, check_timestamp=None, rc=0, changed=True):

    response = hosts.all.ims_catalog_populate(
        online_batch=online_batch,
        dbrc=dbrc,
        ims_id=ims_id,
        irlm_id=irlm_id,
        psb_lib=psb_lib,
        dbd_lib=dbd_lib,
        acb_lib=acb_lib,
        steplib=steplib,
        reslib=reslib,
        modstat=modstat,
        proclib=proclib,
        sysabend=sysabend,
        check_timestamp=check_timestamp,
        buffer_pool_param_dataset=buffer_pool_param_dataset,
        mode=mode,
        control_statements=control_statements,
        bootstrap_dataset=bootstrap_dataset,
        directory_datasets=directory_datasets,
        temp_acb_dataset=temp_acb_dataset,
        directory_staging_dataset=directory_staging_dataset,
        primary_log_dataset=primary_log_dataset,
        secondary_log_dataset=secondary_log_dataset
    )
    for result in response.contacted.values():
        pprint(result)
        assert result['rc'] == rc
        if rc == 0:
            assert validation_msg in result['content']
        else:
            assert validation_msg in result['msg']


def purge_catalog(hosts, validation_msg, primary_log_dataset, psb_lib, dbd_lib, steplib, reslib, proclib,
                  buffer_pool_param_dataset, online_batch=None, dbrc=None, ims_id=None, irlm_id=None, sysut1=None, update_retention_criteria=None,
                  delete=None, managed_acbs=None, delete_dbd_by_version=None, resource_chkp_freq=None, mode='PURGE', rc=0, changed=True):

    response = hosts.all.ims_catalog_purge(
        online_batch=online_batch,
        dbrc=dbrc,
        ims_id=ims_id,
        irlm_id=irlm_id,
        psb_lib=psb_lib,
        dbd_lib=dbd_lib,
        steplib=steplib,
        resource_chkp_freq=resource_chkp_freq,
        reslib=reslib,
        proclib=proclib,
        update_retention_criteria=update_retention_criteria,
        delete_dbd_by_version=delete_dbd_by_version,
        buffer_pool_param_dataset=buffer_pool_param_dataset,
        mode=mode,
        primary_log_dataset=primary_log_dataset,
        delete=delete,
        managed_acbs=managed_acbs,
        sysut1=sysut1
    )
    for result in response.contacted.values():
        pprint(result)
        assert result['rc'] == rc
        if rc == 0:
            assert validation_msg in result['content']
        else:
            assert validation_msg in result['msg']


class CatalogInputParameters():
    HLQ1 = "IMSTESTL."
    HLQ2 = "IMS1."
    PSBSOURCE = "IMSTESTL.ANSIBLE.PSB.SRC"
    DBDSOURCE = "IMSTESTL.ANSIBLE.DBD.SRC"
    ACBDEST = HLQ1 + HLQ2 + "ACBLIB"
    PSBDEST = HLQ1 + HLQ2 + "PSBLIB"
    DBDDEST = HLQ1 + HLQ2 + "DBDLIB"
    PSBLIB = [HLQ1 + HLQ2 + "PSBLIB"]
    DBDLIB = [HLQ1 + HLQ2 + "DBDLIB"]
    ACBLIB = [HLQ1 + HLQ2 + "ACBLIB"]
    STEPLIB = [HLQ1 + HLQ2 + "SDFSRESL"]
    PROCLIB = HLQ1 + HLQ2 + "PROCLIB"
    RESLIB = [HLQ1 + HLQ2 + "SDFSRESL"]
    BUFFERPOOL = HLQ1 + HLQ2 + "PROCLIB(DFSVSMHP)"
    MODSTAT = HLQ1 + HLQ2 + "MODSTAT"
    PSB_NAME = ["PGSAM1"]
    LOADMODE = "LOAD"
    UPDATEMODE = "UPDATE"
    PRIMARYLOG = {
        "dataset_name": HLQ1 + HLQ2 + "LOG",
        "disposition": "NEW",
        "normal_disposition": "DELETE",
        "record_format": "FB",
        "record_length": "4092",
        "block_size": "4096",
        "primary": "100",
        "primary_unit": "CYL",
        "secondary": "75",
        "secondary_unit": "CYL",
        "type": "SEQ"
    }
    PURGEMODE = "PURGE"
    SYSUT1 = {
        "dataset_name": "IMSTESTL.IMS1.ASYS",
        "disposition": "NEW",
        "normal_disposition": "DELETE",
        "type": "SEQ"
    }
    ANALYSISMODE = "ANALYSIS"
    TEMP_ACB = HLQ1 + HLQ2 + "ACB.TEMP"
    STAGE = HLQ1 + HLQ2 + "DFSCD000.STG"
    BSDS = HLQ1 + HLQ2 + "DFSCD000.BSDS"
    DIR1 = HLQ1 + HLQ2 + "DFSCD000.DI1001"
    DIR2 = HLQ1 + HLQ2 + "DFSCD000.DI1002"
    DIR3 = HLQ1 + HLQ2 + "DFSCD000.DI1003"
    DIR_BATCH = [
        {
            'name': DIR1,
            'state': 'absent',
            'volumes': '222222'
        },
        {
            'name': DIR2,
            'state': 'absent',
            'volumes': '222222'
        }
    ]
    RETENTION = [
        {
            'resource': 'DBD',
            'member_name': "*",
            'instances': '0',
            'days': '0'
        },
        {
            'resource': 'PSB',
            'member_name': "*",
            'instances': '0',
            'days': '0'
        }
    ]
    DELETES = [
        {
            "resource": "DBD",
            "member_name": "DB*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "AUTO*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "DF*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "DG*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "DI*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "EMP*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "IP*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "IV*",
            "time_stamp": '*'
        },
        {
            "resource": "DBD",
            "member_name": "SI*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "AUT*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "DBF*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "DFH*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "DFSI*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "DFSS*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "IP*",
            "time_stamp": '*'
        },
        {
            "resource": "PSB",
            "member_name": "PG*",
            "time_stamp": '*'
        }

    ]
