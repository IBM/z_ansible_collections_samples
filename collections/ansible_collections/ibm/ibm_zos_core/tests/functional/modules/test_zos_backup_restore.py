# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
from re import search, IGNORECASE, MULTILINE


VOLUME = "222222"
VOLUME2 = "222222"
VOLUME_TO_BACKUP = VOLUME
BIG_VOLUME = "DSHRL1"
BIG_VOLUME2 = "DSHRL2"
DATA_SET_NAME = "USER.PRIVATE.TESTDS"
DATA_SET_NAME2 = "USER.PRIVATE.TESTDS2"
DATA_SET_PATTERN = "USER.PRIVATE.*"
DATA_SET_CONTENTS = "HELLO world"
DATA_SET_QUALIFIER = "{0}.PRIVATE.TESTDS"
DATA_SET_QUALIFIER2 = "{0}.PRIVATE.TESTDS2"
DATA_SET_BACKUP_LOCATION = "MY.BACKUP"
UNIX_BACKUP_LOCATION = "/tmp/mybackup.dzp"
NEW_HLQ = "NEWHLQ"
DATA_SET_RESTORE_LOCATION = DATA_SET_QUALIFIER.format(NEW_HLQ)
DATA_SET_RESTORE_LOCATION2 = DATA_SET_QUALIFIER2.format(NEW_HLQ)

# ---------------------------------------------------------------------------- #
#                               Helper functions                               #
# ---------------------------------------------------------------------------- #


def create_data_set_or_file_with_contents(hosts, name, contents):
    if name.startswith("/"):
        create_file_with_contents(hosts, name, contents)
    else:
        create_sequential_data_set_with_contents(hosts, name, contents)


def create_sequential_data_set_with_contents(
    hosts, data_set_name, contents, volume=None
):
    if volume is not None:
        results = hosts.all.zos_data_set(name=data_set_name, type="seq", volumes=volume)
    else:
        results = hosts.all.zos_data_set(name=data_set_name, type="seq")
    assert_module_did_not_fail(results)
    results = hosts.all.shell("decho '{0}' {1}".format(contents, data_set_name))
    assert_module_did_not_fail(results)


def create_file_with_contents(hosts, path, contents):
    results = hosts.all.shell("echo '{0}' > {1}".format(contents, path))
    assert_module_did_not_fail(results)


def delete_data_set_or_file(hosts, name):
    if name.startswith("/"):
        delete_file(hosts, name)
    else:
        delete_data_set(hosts, name)


def delete_data_set(hosts, data_set_name):
    hosts.all.zos_data_set(name=data_set_name, state="absent")


def delete_file(hosts, path):
    hosts.all.file(path=path, state="absent")


def assert_module_did_not_fail(results):
    for result in results.contacted.values():
        assert (
            result.get("failed", False) is not True
            and not result.get("exception", "")
            and "error" not in result.get("msg", "").lower()
        )


def assert_module_failed(results):
    for result in results.contacted.values():
        assert (
            result.get("failed", False) is True
            or result.get("exception", "")
            or "error" in result.get("msg", "").lower()
        )


def assert_data_set_or_file_exists(hosts, name):
    if name.startswith("/"):
        assert_file_exists(hosts, name)
    else:
        assert_data_set_exists(hosts, name)


def assert_data_set_exists(hosts, data_set_name):
    results = hosts.all.shell("dls '{0}'".format(data_set_name.upper()))
    for result in results.contacted.values():
        found = search(
            "^{0}$".format(data_set_name), result.get("stdout"), IGNORECASE | MULTILINE
        )
        assert found


def assert_data_set_does_not_exists(hosts, data_set_name):
    results = hosts.all.shell("dls '{0}'".format(data_set_name.upper()))
    for result in results.contacted.values():
        found = search(
            "^{0}$".format(data_set_name), result.get("stdout"), IGNORECASE | MULTILINE
        )
        assert not found


def assert_data_set_exists_on_volume(hosts, data_set_name, volume):
    results = hosts.all.shell("dls -s '{0}'".format(data_set_name.upper()))
    for result in results.contacted.values():
        found = search(
            r"^"
            + data_set_name
            + r"\s+[A-Z]+\s+[A-Z]+\s+[0-9]+\s+"
            + volume
            + r"\s+[0-9]+\s[0-9]+$",
            result.get("stdout"),
            IGNORECASE | MULTILINE,
        )
        assert not found


def assert_file_exists(hosts, path):
    results = hosts.all.stat(path=path)
    for result in results.contacted.values():
        assert result.get("stat").get("exists") is True


# ---------------------------------------------------------------------------- #
#                                Start of tests                                #
# ---------------------------------------------------------------------------- #


@pytest.mark.parametrize(
    "backup_name,overwrite,recover",
    [
        (DATA_SET_BACKUP_LOCATION, False, False),
        (DATA_SET_BACKUP_LOCATION, True, True),
        (DATA_SET_BACKUP_LOCATION, False, True),
        (DATA_SET_BACKUP_LOCATION, True, False),
        (UNIX_BACKUP_LOCATION, False, False),
        (UNIX_BACKUP_LOCATION, True, True),
        (UNIX_BACKUP_LOCATION, False, True),
        (UNIX_BACKUP_LOCATION, True, False),
    ],
)
def test_backup_of_data_set(ansible_zos_module, backup_name, overwrite, recover):
    hosts = ansible_zos_module
    try:
        if not overwrite:
            delete_data_set_or_file(hosts, backup_name)
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        results = hosts.all.zos_backup_restore(
            operation="backup",
            data_sets=dict(include=DATA_SET_NAME),
            backup_name=backup_name,
            overwrite=overwrite,
            recover=recover,
        )
        assert_module_did_not_fail(results)
        assert_data_set_or_file_exists(hosts, backup_name)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, backup_name)


@pytest.mark.parametrize(
    "backup_name,overwrite",
    [
        (DATA_SET_BACKUP_LOCATION, False),
        (DATA_SET_BACKUP_LOCATION, True),
        (UNIX_BACKUP_LOCATION, False),
        (UNIX_BACKUP_LOCATION, True),
    ],
)
def test_backup_of_data_set_when_backup_dest_exists(
    ansible_zos_module, backup_name, overwrite
):
    hosts = ansible_zos_module
    try:
        create_data_set_or_file_with_contents(hosts, backup_name, DATA_SET_CONTENTS)
        assert_data_set_or_file_exists(hosts, backup_name)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        results = hosts.all.zos_backup_restore(
            operation="backup",
            data_sets=dict(include=DATA_SET_NAME),
            backup_name=backup_name,
            overwrite=overwrite,
        )
        if overwrite:
            assert_module_did_not_fail(results)
        else:
            assert_module_failed(results)
        assert_data_set_or_file_exists(hosts, backup_name)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, backup_name)


@pytest.mark.parametrize(
    "backup_name,overwrite,recover",
    [
        (DATA_SET_BACKUP_LOCATION, False, False),
        (DATA_SET_BACKUP_LOCATION, True, True),
        (DATA_SET_BACKUP_LOCATION, False, True),
        (DATA_SET_BACKUP_LOCATION, True, False),
        (UNIX_BACKUP_LOCATION, False, False),
        (UNIX_BACKUP_LOCATION, True, True),
        (UNIX_BACKUP_LOCATION, False, True),
        (UNIX_BACKUP_LOCATION, True, False),
    ],
)
def test_backup_and_restore_of_data_set(
    ansible_zos_module, backup_name, overwrite, recover
):
    hosts = ansible_zos_module
    try:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, backup_name)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        results = hosts.all.zos_backup_restore(
            operation="backup",
            data_sets=dict(include=DATA_SET_NAME),
            backup_name=backup_name,
            overwrite=overwrite,
            recover=recover,
        )
        assert_module_did_not_fail(results)
        assert_data_set_or_file_exists(hosts, backup_name)
        results = hosts.all.zos_backup_restore(
            operation="restore",
            backup_name=backup_name,
            hlq=NEW_HLQ,
            overwrite=overwrite,
        )
        assert_module_did_not_fail(results)
        assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, backup_name)


@pytest.mark.parametrize(
    "backup_name,space,space_type",
    [
        (DATA_SET_BACKUP_LOCATION, 10, "M"),
        (DATA_SET_BACKUP_LOCATION, 10000, "K"),
        (DATA_SET_BACKUP_LOCATION, 10, None),
        (DATA_SET_BACKUP_LOCATION, 2, "CYL"),
        (DATA_SET_BACKUP_LOCATION, 10, "TRK"),
        (UNIX_BACKUP_LOCATION, 10, "M"),
        (UNIX_BACKUP_LOCATION, 10000, "K"),
        (UNIX_BACKUP_LOCATION, 10, None),
        (UNIX_BACKUP_LOCATION, 2, "CYL"),
        (UNIX_BACKUP_LOCATION, 10, "TRK"),
    ],
)
def test_backup_and_restore_of_data_set_various_space_measurements(
    ansible_zos_module, backup_name, space, space_type
):
    hosts = ansible_zos_module
    try:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, backup_name)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        args = dict(
            operation="backup",
            data_sets=dict(include=DATA_SET_NAME),
            backup_name=backup_name,
            overwrite=True,
            space=space,
        )
        if space_type:
            args["space_type"] = space_type
        results = hosts.all.zos_backup_restore(**args)
        assert_module_did_not_fail(results)
        assert_data_set_or_file_exists(hosts, backup_name)
        args = dict(
            operation="restore",
            backup_name=backup_name,
            hlq=NEW_HLQ,
            overwrite=True,
            space=space,
        )
        if space_type:
            args["space_type"] = space_type
        results = hosts.all.zos_backup_restore(**args)
        assert_module_did_not_fail(results)
        assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, backup_name)


@pytest.mark.parametrize(
    "backup_name,overwrite",
    [
        (DATA_SET_BACKUP_LOCATION, False),
        (DATA_SET_BACKUP_LOCATION, True),
        (UNIX_BACKUP_LOCATION, False),
        (UNIX_BACKUP_LOCATION, True),
    ],
)
def test_backup_and_restore_of_data_set_when_restore_location_exists(
    ansible_zos_module, backup_name, overwrite
):
    hosts = ansible_zos_module
    try:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, backup_name)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        results = hosts.all.zos_backup_restore(
            operation="backup",
            data_sets=dict(include=DATA_SET_NAME),
            backup_name=backup_name,
        )
        assert_module_did_not_fail(results)
        assert_data_set_or_file_exists(hosts, backup_name)
        results = hosts.all.zos_backup_restore(
            operation="restore",
            backup_name=backup_name,
            hlq=NEW_HLQ,
        )
        assert_module_did_not_fail(results)
        assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION)
        results = hosts.all.zos_backup_restore(
            operation="restore",
            backup_name=backup_name,
            hlq=NEW_HLQ,
            overwrite=overwrite,
        )
        if overwrite:
            assert_module_did_not_fail(results)
        else:
            assert_module_failed(results)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, backup_name)


@pytest.mark.parametrize(
    "data_set_include",
    [
        [DATA_SET_NAME, DATA_SET_NAME2],
        DATA_SET_PATTERN,
    ],
)
def test_backup_and_restore_of_multiple_data_sets(ansible_zos_module, data_set_include):
    hosts = ansible_zos_module
    try:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_NAME2)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION2)
        delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME2, DATA_SET_CONTENTS
        )
        results = hosts.all.zos_backup_restore(
            operation="backup",
            data_sets=dict(include=data_set_include),
            backup_name=DATA_SET_BACKUP_LOCATION,
        )
        assert_module_did_not_fail(results)
        assert_data_set_or_file_exists(hosts, DATA_SET_BACKUP_LOCATION)
        results = hosts.all.zos_backup_restore(
            operation="restore",
            backup_name=DATA_SET_BACKUP_LOCATION,
            overwrite=True,
            recover=True,
            hlq=NEW_HLQ,
        )
        assert_module_did_not_fail(results)
        assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION)
        assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION2)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_NAME2)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION2)
        delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)


def test_backup_and_restore_exclude_from_pattern(ansible_zos_module):
    hosts = ansible_zos_module
    try:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_NAME2)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION2)
        delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME, DATA_SET_CONTENTS
        )
        create_sequential_data_set_with_contents(
            hosts, DATA_SET_NAME2, DATA_SET_CONTENTS
        )
        results = hosts.all.zos_backup_restore(
            operation="backup",
            data_sets=dict(include=DATA_SET_PATTERN, exclude=DATA_SET_NAME2),
            backup_name=DATA_SET_BACKUP_LOCATION,
        )
        assert_module_did_not_fail(results)
        assert_data_set_or_file_exists(hosts, DATA_SET_BACKUP_LOCATION)
        results = hosts.all.zos_backup_restore(
            operation="restore",
            backup_name=DATA_SET_BACKUP_LOCATION,
            overwrite=True,
            recover=True,
            hlq=NEW_HLQ,
        )
        assert_module_did_not_fail(results)
        assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION)
        assert_data_set_does_not_exists(hosts, DATA_SET_RESTORE_LOCATION2)
    finally:
        delete_data_set_or_file(hosts, DATA_SET_NAME)
        delete_data_set_or_file(hosts, DATA_SET_NAME2)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
        delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION2)
        delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)


# def test_backup_and_restore_of_data_set_from_volume_to_new_volume(ansible_zos_module):
#     hosts = ansible_zos_module
#     try:
#         delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)
#         delete_data_set_or_file(hosts, DATA_SET_NAME)
#         delete_data_set_or_file(hosts, DATA_SET_NAME2)
#         delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
#         delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION2)
#         create_sequential_data_set_with_contents(
#             hosts, DATA_SET_NAME, DATA_SET_CONTENTS, VOLUME
#         )
#         create_sequential_data_set_with_contents(
#             hosts, DATA_SET_NAME2, DATA_SET_CONTENTS, VOLUME2
#         )
#         results = hosts.all.zos_backup_restore(
#             operation="backup",
#             data_sets=dict(include=DATA_SET_PATTERN),
#             volume=VOLUME,
#             backup_name=DATA_SET_BACKUP_LOCATION,
#             overwrite=True,
#         )
#         assert_module_did_not_fail(results)
#         assert_data_set_or_file_exists(hosts, DATA_SET_BACKUP_LOCATION)
#         results = hosts.all.zos_backup_restore(
#             operation="restore",
#             backup_name=DATA_SET_BACKUP_LOCATION,
#             overwrite=True,
#             volume=VOLUME,
#             hlq=NEW_HLQ,
#         )
#         assert_module_did_not_fail(results)
#         assert_data_set_exists(hosts, DATA_SET_RESTORE_LOCATION)
#         assert_data_set_does_not_exists(hosts, DATA_SET_RESTORE_LOCATION2)
#     finally:
#         delete_data_set_or_file(hosts, DATA_SET_NAME)
#         delete_data_set_or_file(hosts, DATA_SET_NAME2)
#         delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION)
#         delete_data_set_or_file(hosts, DATA_SET_RESTORE_LOCATION2)
#         delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)


# def test_backup_and_restore_of_full_volume(ansible_zos_module):
#     hosts = ansible_zos_module
#     try:
#         delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)
#         delete_data_set_or_file(hosts, DATA_SET_NAME)
#         create_sequential_data_set_with_contents(
#             hosts, DATA_SET_NAME, DATA_SET_CONTENTS, VOLUME
#         )
#         results = hosts.all.zos_backup_restore(
#             operation="backup",
#             volume=VOLUME,
#             full_volume=True,
#             sms_storage_class="DB2SMS10",
#             backup_name=DATA_SET_BACKUP_LOCATION,
#             overwrite=True,
#             space=500,
#             space_type="M",
#         )
#         assert_module_did_not_fail(results)
#         assert_data_set_or_file_exists(hosts, DATA_SET_BACKUP_LOCATION)
#         delete_data_set_or_file(hosts, DATA_SET_NAME)
#         results = hosts.all.zos_backup_restore(
#             operation="restore",
#             backup_name=DATA_SET_BACKUP_LOCATION,
#             overwrite=True,
#             volume=VOLUME,
#             full_volume=True,
#             sms_storage_class="DB2SMS10",
#             space=500,
#             space_type="M",
#         )
#         assert_module_did_not_fail(results)
#         assert_data_set_exists_on_volume(hosts, DATA_SET_NAME, VOLUME)
#     finally:
#         delete_data_set_or_file(hosts, DATA_SET_NAME)
#         delete_data_set_or_file(hosts, DATA_SET_BACKUP_LOCATION)
