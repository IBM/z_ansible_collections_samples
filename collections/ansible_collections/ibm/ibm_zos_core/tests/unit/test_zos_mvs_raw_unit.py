# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest

IMPORT_NAME = "ibm_zos_core.plugins.modules.zos_mvs_raw"


class DummyModule(object):
    """Used in place of Ansible's module
    so we can easily mock the desired behavior."""

    def __init__(self, rc=0, stdout="", stderr=""):
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr

    def run_command(self, *args, **kwargs):
        return (self.rc, self.stdout, self.stderr)


@pytest.mark.parametrize(
    (
        "data_set_name,"
        "disposition,"
        "disposition_normal,"
        "disposition_abnormal,"
        "space_type,"
        "space_primary,"
        "space_secondary,"
        "sms_management_class,"
        "sms_data_class,"
        "sms_storage_class,"
        "block_size,"
        "key_label,"
        "type,"
        "encryption_key_1,"
        "encryption_key_2,"
        "record_format"
    ),
    [
        (
            "data.set.name",
            "new",
            "keep",
            "keep",
            "cyl",
            5,
            1,
            "smsclas1",
            "smsclas1",
            "smsclas1",
            80,
            "SOMEKEYLAB100",
            "library",
            {"label": "keyforme", "encoding": "h"},
            {"label": "keyforme2", "encoding": "h"},
            "u",
        ),
        (
            "data.set.name(mem1)",
            "shr",
            "delete",
            "keep",
            "trk",
            "5",
            1,
            "smsclas1",
            "smsclas2",
            "smsclas3",
            120,
            "somekeylab1",
            "basic",
            {"label": "keyforme", "encoding": "l"},
            {"label": "keyforme2", "encoding": "h"},
            "fb",
        ),
        (
            "DATA.NAME.HERE.NOW",
            "old",
            "catalog",
            "uncatalog",
            "b",
            55,
            "100",
            "SMSCLASS",
            "smsclas2",
            "smscD@s3",
            120,
            "keyfor342fdsme",
            "large",
            {"label": "keyforME", "encoding": "l"},
            {"label": "KEY4me", "encoding": "h"},
            "fba",
        ),
        (
            "DAT@.now",
            "mod",
            "delete",
            "uncatalog",
            "g",
            1,
            "9",
            "SMSCLASS",
            "smsclas2",
            "",
            120,
            "keyfor342fdsme",
            "pdse",
            {"label": "keyforME", "encoding": "l"},
            {"label": "KEY4me", "encoding": "h"},
            "vb",
        ),
        (
            "DAT$.now",
            "new",
            "delete",
            "keep",
            "m",
            1,
            9,
            "SMSCLASS",
            "smsclas2",
            "",
            0,
            "",
            "lds",
            {"label": "keyforME", "encoding": "l"},
            {"label": "keyyyyy343asdfasfsdfa", "encoding": "l"},
            "vba",
        ),
    ],
)
def test_argument_parsing_data_set(
    zos_import_mocker,
    data_set_name,
    disposition,
    disposition_normal,
    disposition_abnormal,
    space_type,
    space_primary,
    space_secondary,
    sms_management_class,
    sms_data_class,
    sms_storage_class,
    block_size,
    key_label,
    type,
    encryption_key_1,
    encryption_key_2,
    record_format,
):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "{0}.AnsibleModule".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    mocker.patch(
        "{0}.AnsibleModuleHelper".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_data_set": {
                    "dd_name": "MYDD1",
                    "data_set_name": data_set_name,
                    "disposition": disposition,
                    "disposition_normal": disposition_normal,
                    "disposition_abnormal": disposition_abnormal,
                    "space_type": space_type,
                    "space_primary": space_primary,
                    "space_secondary": space_secondary,
                    "sms_management_class": sms_management_class,
                    "sms_storage_class": sms_storage_class,
                    "sms_data_class": sms_data_class,
                    "block_size": block_size,
                    "key_label": key_label,
                    "type": type,
                    "encryption_key_1": encryption_key_1,
                    "encryption_key_2": encryption_key_2,
                    "record_format": record_format,
                    "return_content": {"type": "text"},
                    "reuse": False,
                    "replace": False,
                    "backup": False,
                }
            },
        ],
    }
    error_raised = False
    try:
        raw.parse_and_validate_args(valid_args)
    except Exception as e:
        print(str(e))
        error_raised = True
    assert not error_raised


@pytest.mark.parametrize(
    (
        "path,"
        "disposition_normal,"
        "disposition_abnormal,"
        "block_size,"
        "record_length,"
        "record_format,"
        "file_data_type,"
        "access_group,"
        "status_group,"
    ),
    [
        (
            "/u/omvsadm",
            "keep",
            "delete",
            0,
            100,
            "fb",
            "record",
            "r",
            ["ocreat", "oappend", "onoctty"],
        ),
        (
            "/u/omvsadm",
            "delete",
            "delete",
            200,
            "100",
            "fba",
            "record",
            "w",
            ["oappend", "osync"],
        ),
        ("/u/OEUSR01", "keep", "delete", 0, 100, "vb", "binary", "rw", ["ononblock"]),
        ("/u/testmeee", "keep", "delete", 0, 100, "vba", "record", "read_only", []),
        ("/u/hellow/d/or4ld", "keep", "keep", 0, 100, "u", "text", "write_only", []),
    ],
)
def test_argument_parsing_unix(
    zos_import_mocker,
    path,
    disposition_normal,
    disposition_abnormal,
    block_size,
    record_length,
    record_format,
    file_data_type,
    access_group,
    status_group,
):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "{0}.AnsibleModule".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    mocker.patch(
        "{0}.AnsibleModuleHelper".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_unix": {
                    "dd_name": "mydd2",
                    "path": path,
                    "disposition_normal": disposition_normal,
                    "disposition_abnormal": disposition_abnormal,
                    "file_data_type": file_data_type,
                    "record_length": record_length,
                    "block_size": 80,
                    "record_format": record_format,
                    "access_group": access_group,
                    "status_group": status_group,
                    "return_content": {"type": "text"},
                }
            },
        ],
    }
    error_raised = False
    try:
        raw.parse_and_validate_args(valid_args)
    except Exception as e:
        print(str(e))
        error_raised = True
    assert not error_raised


@pytest.mark.parametrize(
    (
        "data_set_name,"
        "disposition,"
        "disposition_normal,"
        "disposition_abnormal,"
        "space_type,"
        "space_primary,"
        "space_secondary,"
        "sms_management_class,"
        "sms_data_class,"
        "sms_storage_class,"
        "block_size,"
        "key_label,"
        "type,"
        "encryption_key_1,"
        "encryption_key_2,"
        "record_format"
    ),
    [
        (
            "4data.set.name",
            "old",
            "keep",
            "keep",
            "cyl",
            5,
            1,
            "smsclas1",
            "smsclas1",
            "smsclas1",
            80,
            "SOMEKEYLAB100",
            "library",
            {"label": "keyforme", "encoding": "h"},
            {"label": "keyforme2", "encoding": "h"},
            "u",
        ),
        (
            "data.set.name(mem1waytoolong)",
            "excl",
            "delete",
            "keep",
            "trk",
            "5",
            1,
            "smsclas1",
            "smsclas2",
            "smsclas3",
            120,
            "somekeylab1",
            "basic",
            {"label": "keyforme", "encoding": "l"},
            {"label": "keyforme2", "encoding": "h"},
            "fb",
        ),
        (
            "DATA.NAME.HERE.NOW",
            "old",
            "catalog",
            "uncatalog",
            "gb",
            55,
            "100",
            "SMSCLASS",
            "smsclas2",
            "smscD@s3",
            120,
            "keyfor342fdsme",
            "large",
            {"label": "keyforME", "encoding": "l"},
            {"label": "KEY4me", "encoding": "h"},
            "fba",
        ),
        (
            "DAT@.now",
            "mod",
            "delete",
            "uncatalog",
            "g",
            1,
            "9",
            "SMSCLASSsss",
            "smsclas2",
            "",
            120,
            "keyfor342fdsme",
            "pdse",
            {"label": "keyforME", "encoding": "l"},
            {"label": "KEY4me", "encoding": "h"},
            "vb",
        ),
        (
            "DAT$.now",
            "new",
            "delete",
            "meep",
            "m",
            1,
            9,
            "SMSCLASS",
            "smsclas2",
            "",
            0,
            "",
            "ksdss",
            {"label": "keyforME", "encoding": "l"},
            {"label": "keyyyyy343asdfasfsdfa", "encoding": "l"},
            "vba",
        ),
    ],
)
def test_argument_parsing_data_set_failure_path(
    zos_import_mocker,
    data_set_name,
    disposition,
    disposition_normal,
    disposition_abnormal,
    space_type,
    space_primary,
    space_secondary,
    sms_management_class,
    sms_data_class,
    sms_storage_class,
    block_size,
    key_label,
    type,
    encryption_key_1,
    encryption_key_2,
    record_format,
):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "{0}.AnsibleModule".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    mocker.patch(
        "{0}.AnsibleModuleHelper".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_data_set": {
                    "dd_name": "MYDD1",
                    "data_set_name": data_set_name,
                    "disposition": disposition,
                    "disposition_normal": disposition_normal,
                    "disposition_abnormal": disposition_abnormal,
                    "space_type": space_type,
                    "space_primary": space_primary,
                    "space_secondary": space_secondary,
                    "sms_management_class": sms_management_class,
                    "sms_storage_class": sms_storage_class,
                    "sms_data_class": sms_data_class,
                    "block_size": block_size,
                    "key_label": key_label,
                    "type": type,
                    "encryption_key_1": encryption_key_1,
                    "encryption_key_2": encryption_key_2,
                    "record_format": record_format,
                    "return_content": {"type": "text"},
                    "reuse": False,
                    "replace": False,
                    "backup": False,
                }
            },
        ],
    }
    with pytest.raises(ValueError):
        raw.parse_and_validate_args(valid_args)


@pytest.mark.parametrize(
    (
        "path,"
        "disposition_normal,"
        "disposition_abnormal,"
        "block_size,"
        "record_length,"
        "record_format,"
        "file_data_type,"
        "access_group,"
        "status_group,"
    ),
    [
        (
            "../u/omvsadm",
            "keep",
            "delete",
            0,
            100,
            "fb",
            "record",
            "r",
            ["ocreat", "oappend", "onoctty"],
        ),
        (
            "/u/omvsadm",
            "delete",
            "delete",
            200,
            "100",
            "fba",
            "record",
            "w",
            ["append", "osync"],
        ),
        ("/u/OEUSR01", "keep", "catalog", 0, 100, "vb", "binary", "rw", ["ononblock"]),
        (
            "/u/testmeee",
            "keep",
            "delete",
            0,
            100,
            "vba",
            "record",
            "read_only",
            ["hello"],
        ),
        ("/u/hellow/d/or4ld", "meep", "keep", 0, 100, "u", "text", None, []),
    ],
)
def test_argument_parsing_unix_failure_path(
    zos_import_mocker,
    path,
    disposition_normal,
    disposition_abnormal,
    block_size,
    record_length,
    record_format,
    file_data_type,
    access_group,
    status_group,
):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "ansible.module_utils.basic.AnsibleModule.run_command",
        create=True,
        return_value=DummyModule(),
    )
    mocker.patch(
        "ansible.module_utils.basic.AnsibleModule.__init__",
        create=True,
        return_value=None,
    )
    mocker.patch(
        "{0}.AnsibleModuleHelper".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_unix": {
                    "dd_name": "mydd2",
                    "path": path,
                    "disposition_normal": disposition_normal,
                    "disposition_abnormal": disposition_abnormal,
                    "file_data_type": file_data_type,
                    "record_length": record_length,
                    "block_size": 80,
                    "record_format": record_format,
                    "access_group": access_group,
                    "status_group": status_group,
                    "return_content": {"type": "text"},
                }
            },
        ],
    }
    with pytest.raises(ValueError):
        raw.parse_and_validate_args(valid_args)


def test_ksds_defaults(zos_import_mocker,):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "{0}.AnsibleModule".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_data_set": {
                    "dd_name": "MYDD1",
                    "data_set_name": "my.ds",
                    "disposition": "new",
                    "type": "ksds",
                }
            },
        ],
    }
    error_raised = False
    parsed_args = {}
    try:
        parsed_args = raw.parse_and_validate_args(valid_args)
    except Exception as e:
        print(str(e))
        error_raised = True
    assert not error_raised
    assert (
        parsed_args.get("dds", [{}])[0].get("dd_data_set", {}).get("key_length", -1)
        == 5
    )
    assert (
        parsed_args.get("dds", [{}])[0].get("dd_data_set", {}).get("key_offset", -1)
        == 0
    )


def test_ksds_exception_key_length(zos_import_mocker,):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "{0}.AnsibleModule".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_data_set": {
                    "dd_name": "MYDD1",
                    "data_set_name": "my.ds",
                    "disposition": "new",
                    "type": "esds",
                    "key_length": 5,
                }
            },
        ],
    }
    with pytest.raises(ValueError):
        raw.parse_and_validate_args(valid_args)


def test_ksds_exception_key_offset(zos_import_mocker,):
    mocker, importer = zos_import_mocker
    raw = importer(IMPORT_NAME)
    mocker.patch(
        "{0}.AnsibleModule".format(IMPORT_NAME),
        create=True,
        return_value=DummyModule(),
    )
    valid_args = {
        "program_name": "idcams",
        "auth": True,
        "parm": "SOMEPARM",
        "dds": [
            {
                "dd_data_set": {
                    "dd_name": "MYDD1",
                    "data_set_name": "my.ds",
                    "disposition": "new",
                    "type": "esds",
                    "key_offset": 5,
                }
            },
        ],
    }
    with pytest.raises(ValueError):
        raw.parse_and_validate_args(valid_args)
