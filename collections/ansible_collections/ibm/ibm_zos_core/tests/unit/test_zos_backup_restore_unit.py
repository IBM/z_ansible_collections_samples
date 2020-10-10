# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest

IMPORT_NAME = "ibm_zos_core.plugins.modules.zos_backup_restore"


class DummyModule(object):
    """Used in place of Ansible's module
    so we can easily mock the desired behavior."""

    def __init__(self, rc=0, stdout="", stderr=""):
        self.rc = rc
        self.stdout = stdout
        self.stderr = stderr

    def run_command(self, *args, **kwargs):
        return (self.rc, self.stdout, self.stderr)


@pytest.fixture(scope="function")
def zos_backup_restore_mocker(zos_import_mocker):
    """Pytest fixture in charge of patching unavailable imports
    so we can run z/OS module test cases on x86 for zos_backup_restore.

    Args:
        zos_import_mocker (zos_import_mocker): A pytest fixture

    Yields:
        object: The zos_backup_restore module object
    """
    mocker, importer = zos_import_mocker
    zos_backup_restore = importer(IMPORT_NAME)
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
    yield zos_backup_restore


def assert_args_valid(zos_backup_restore, arguments):
    """Asserts arguments DO NOT cause parser to raise exception.

    Args:
        zos_backup_restore (zos_backup_restore_mocker): Mocker object which provides access to functions in zos_backup_restore module.
        valid_args (dict): The arguments to pass to parser.
    """
    error_raised = False
    try:
        zos_backup_restore.parse_and_validate_args(arguments)
    except Exception as e:
        print(str(e))
        error_raised = True
    assert not error_raised


def assert_args_invalid(zos_backup_restore, arguments):
    """Asserts arguments DO cause parser to raise exception.

    Args:
        zos_backup_restore (zos_backup_restore_mocker): Mocker object which provides access to functions in zos_backup_restore module.
        valid_args (dict): The arguments to pass to parser.
    """
    error_raised = False
    try:
        zos_backup_restore.parse_and_validate_args(arguments)
    except Exception as e:
        print(repr(e))
        error_raised = True
    assert error_raised


@pytest.mark.parametrize(
    "space_type", ["K", "M", "G", "TRK", "CYL", "k", "m", "g", "trk", "cyl"]
)
def test_valid_space_types(zos_backup_restore_mocker, space_type):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include="user.*"),
        space=10,
        space_type=space_type,
        backup_name="backup.name.here",
    )
    assert_args_valid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize("operation", ["backup", "restore"])
def test_valid_operation(zos_backup_restore_mocker, operation):
    valid_args = dict(
        operation=operation,
        data_sets=dict(include="user.*"),
        backup_name="backup.name.here",
    )
    if operation == "restore":
        valid_args["hlq"] = "VALIDQ"
    assert_args_valid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize(
    "backup_name",
    [
        "user.private.test",
        "user.PRIVATE",
        "/tmp/mybackup.dzp",
        "/some/very/long/but/totally/valid/path/to/a/file",
        "USER.PR@VATE.TES#T",
    ],
)
def test_valid_backup_name(zos_backup_restore_mocker, backup_name):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include="user.*"),
        backup_name=backup_name,
    )
    assert_args_valid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize(
    "sms_class",
    ["MYIclASS", "M0341CSS", "storagec", "s", "$*@@%#1", "#"],
)
def test_valid_sms_classes(zos_backup_restore_mocker, sms_class):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include="user.*"),
        sms_storage_class=sms_class,
        sms_management_class=sms_class,
    )
    assert_args_valid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize(
    "data_set_pattern",
    [
        "user.**",
        "some.*",
        "user.p?ivate.pr?*",
        "U#@R.PRIVATE.p5234",
        ["user.**", "some.*", "user.p?ivate.pr?*", "U#@R.PRIVATE.p5234"],
    ],
)
def test_valid_data_set_patterns(zos_backup_restore_mocker, data_set_pattern):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include=data_set_pattern, exclude=data_set_pattern),
    )
    assert_args_valid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize("space_type", [12, "Hf", "F", -1, "C"])
def test_invalid_space_types(zos_backup_restore_mocker, space_type):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include="user.*"),
        space=10,
        space_type=space_type,
        backup_name="backup.name.here",
    )
    assert_args_invalid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize("operation", ["BACKUP", "restorE", "keep", -1, True, 100, ""])
def test_invalid_operation(zos_backup_restore_mocker, operation):
    valid_args = dict(
        operation=operation,
        data_sets=dict(include="user.*"),
        backup_name="backup.name.here",
    )
    if operation == "restore":
        valid_args["hlq"] = "VALIDQ"
    assert_args_invalid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize(
    "backup_name",
    [
        "../relative/path",
        "./",
        ".",
        "5bad.data.set",
        "superlongqualifierdefinitelyovermaxlength",
    ],
)
def test_invalid_backup_name(zos_backup_restore_mocker, backup_name):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include="user.*"),
        backup_name=backup_name,
    )
    assert_args_invalid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize(
    "sms_class",
    ["5555bad", "toolongclass", "bad!char", True, False, 100, 0, -1],
)
def test_invalid_sms_classes(zos_backup_restore_mocker, sms_class):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include="user.*"),
        sms_storage_class=sms_class,
        sms_management_class=sms_class,
    )
    assert_args_invalid(zos_backup_restore_mocker, valid_args)


@pytest.mark.parametrize(
    "data_set_pattern",
    [
        "5ser.**",
        "bad!naMe",
        "definitelywaytoolongforadatasethlq",
        "/some/file/path",
        "data.set.member(mem1)",
        True,
        False,
        0,
        1,
        100,
        -100,
    ],
)
def test_invalid_data_set_patterns(zos_backup_restore_mocker, data_set_pattern):
    valid_args = dict(
        operation="backup",
        data_sets=dict(include=data_set_pattern, exclude=data_set_pattern),
    )
    assert_args_invalid(zos_backup_restore_mocker, valid_args)
