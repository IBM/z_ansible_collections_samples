# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
from pprint import pprint

EXISTING_DATA_SET = "user.private.proclib"
DEFAULT_DATA_SET = "user.private.rawds"
DEFAULT_DATA_SET_2 = "user.private.rawds2"
DEFAULT_DATA_SET_WITH_MEMBER = "{0}(mem1)".format(DEFAULT_DATA_SET)
DEFAULT_PATH = "/tmp/ansible/testdir"
DEFAULT_PATH_WITH_FILE = "{0}/testfile".format(DEFAULT_PATH)
DEFAULT_DD = "MYDD"
SYSIN_DD = "SYSIN"
SYSPRINT_DD = "SYSPRINT"
IDCAMS_STDIN = " LISTCAT ENTRIES('{0}')".format(EXISTING_DATA_SET.upper())
IDCAMS_INVALID_STDIN = " hello world #$!@%!#$!@``~~^$*%"
DEFAULT_VOLUME = "000000"


# ---------------------------------------------------------------------------- #
#                               Data set DD tests                              #
# ---------------------------------------------------------------------------- #


def test_failing_name_format(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        dds=[dict(dd_data_set=dict(dd_name=DEFAULT_DD, data_set_name="!!^&.BAD.NAME"))],
    )
    for result in results.contacted.values():
        pprint(result)
        assert "ValueError" in result.get("msg")


def test_disposition_new(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "disposition", ["shr", "mod", "old"],
)
def test_dispositions_for_existing_data_set(ansible_zos_module, disposition):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, type="seq", state="present", replace=True
    )
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition=disposition,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


# * new data set and append to member in one step not currently supported
def test_new_disposition_for_data_set_members(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET_WITH_MEMBER,
                    disposition="new",
                    type="pds",
                    directory_blocks=15,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 8
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", True) is False


@pytest.mark.parametrize(
    "disposition", ["shr", "mod", "old"],
)
def test_dispositions_for_existing_data_set_members(ansible_zos_module, disposition):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, type="pds", state="present", replace=True
    )
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET_WITH_MEMBER,
                    disposition=disposition,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "normal_disposition,changed",
    [("keep", True), ("delete", True), ("catalog", True), ("uncatalog", True)],
)
def test_normal_dispositions_data_set(ansible_zos_module, normal_disposition, changed):
    hosts = ansible_zos_module
    results = hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET,
        type="seq",
        state="present",
        replace=True,
        volumes=[DEFAULT_VOLUME],
    )
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="shr",
                    disposition_normal=normal_disposition,
                    volumes=[DEFAULT_VOLUME],
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, state="absent", volumes=[DEFAULT_VOLUME]
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", not changed) is changed


@pytest.mark.parametrize(
    "space_type,primary,secondary,expected",
    [
        ("trk", 3, 1, 169992),
        ("cyl", 3, 1, 2549880),
        ("b", 3, 1, 56664),
        ("k", 3, 1, 56664),
        ("m", 3, 1, 2889864),
    ],
)
def test_space_types(ansible_zos_module, space_type, primary, secondary, expected):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    space_primary=primary,
                    space_secondary=secondary,
                    space_type=space_type,
                    volumes=[DEFAULT_VOLUME],
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.command(cmd="dls -l -s {0}".format(DEFAULT_DATA_SET))
    for result in results.contacted.values():
        pprint(result)
        assert str(expected) in result.get("stdout", "")

    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "data_set_type", ["pds", "pdse", "large", "basic", "seq"],
)
def test_data_set_types_non_vsam(ansible_zos_module, data_set_type):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type=data_set_type,
                    volumes=[DEFAULT_VOLUME],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert (
            result.get("ret_code", {}).get("code", -1) != 8
            and result.get("ret_code", {}).get("code", -1) != -1
        )
    results = hosts.all.command(cmd="dls {0}".format(DEFAULT_DATA_SET))
    for result in results.contacted.values():
        pprint(result)
        assert "BGYSC1103E" not in result.get("stderr", "")

    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "data_set_type", ["ksds", "rrds", "lds", "esds"],
)
def test_data_set_types_vsam(ansible_zos_module, data_set_type):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            # * ksds requires additional parameters
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type=data_set_type,
                    volumes=[DEFAULT_VOLUME],
                ),
            )
            if data_set_type != "ksds"
            else dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type=data_set_type,
                    key_length=5,
                    key_offset=0,
                    volumes=[DEFAULT_VOLUME],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert (
            result.get("ret_code", {}).get("code", -1) != 8
            and result.get("ret_code", {}).get("code", -1) != -1
        )
    # * we hope to see EDC5041I An error was detected at the system level when opening a file.
    # * because that means data set exists and is VSAM so we can't read it
    results = hosts.all.command(cmd="head \"//'{0}'\"".format(DEFAULT_DATA_SET))
    for result in results.contacted.values():
        pprint(result)
        assert "EDC5041I" in result.get("stderr", "")

    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "record_format", ["u", "vb", "vba", "fb", "fba"],
)
def test_record_formats(ansible_zos_module, record_format):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    record_format=record_format,
                    volumes=[DEFAULT_VOLUME],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) != 8
        assert result.get("ret_code", {}).get("code", -1) != -1

    results = hosts.all.command(cmd="dls -l {0}".format(DEFAULT_DATA_SET))
    for result in results.contacted.values():
        pprint(result)
        assert str(" {0} ".format(record_format.upper())) in result.get("stdout", "")

    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "return_content_type,expected",
    [
        ("text", "IDCAMS  SYSTEM"),
        (
            "base64",
            "\udcc9\udcc4\udcc3\udcc1\udcd4\udce2@@\udce2\udce8\udce2\udce3\udcc5",
        ),
    ],
)
def test_return_content_type(ansible_zos_module, return_content_type, expected):
    hosts = ansible_zos_module
    results = hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET,
        type="seq",
        state="present",
        replace=True,
        volumes=[DEFAULT_VOLUME],
    )
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="shr",
                    volumes=[DEFAULT_VOLUME],
                    return_content=dict(type=return_content_type),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, state="absent", volumes=[DEFAULT_VOLUME]
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert expected in "\n".join(result.get("dd_names")[0].get("content", []))


@pytest.mark.parametrize(
    "src_encoding,response_encoding,expected",
    [
        ("iso8859-1", "ibm-1047", "qcfe\udcebB||BTBFg\udceb|Bg\udcfdGqfgB"),
        ("ibm-1047", "iso8859-1", "IDCAMS  SYSTEM",),
    ],
)
def test_return_text_content_encodings(
    ansible_zos_module, src_encoding, response_encoding, expected
):
    hosts = ansible_zos_module
    results = hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET,
        type="seq",
        state="present",
        replace=True,
        volumes=[DEFAULT_VOLUME],
    )
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="shr",
                    volumes=[DEFAULT_VOLUME],
                    return_content=dict(
                        type="text",
                        src_encoding=src_encoding,
                        response_encoding=response_encoding,
                    ),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, state="absent", volumes=[DEFAULT_VOLUME]
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert expected in "\n".join(result.get("dd_names")[0].get("content", []))


def test_reuse_existing_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, type="seq", state="present", replace=True
    )
    results = hosts.all.zos_mvs_raw(
        program_name="IDCAMS",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    reuse=True,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", 0) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


def test_replace_existing_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(
        name=DEFAULT_DATA_SET, type="seq", state="present", replace=True
    )
    results = hosts.all.zos_mvs_raw(
        program_name="IDCAMS",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    replace=True,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", 0) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


def test_replace_existing_data_set_make_backup(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    hosts.all.zos_mvs_raw(
        program_name="IDCAMS",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    replace=True,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    results = hosts.all.zos_mvs_raw(
        program_name="IDCAMS",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    replace=True,
                    backup=True,
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", 0) == 0
        assert len(result.get("dd_names", [])) > 0
        assert len(result.get("backups", [])) > 0
        assert result.get("backups")[0].get("backup_name") is not None
        results2 = hosts.all.command(
            cmd="head \"//'{0}'\"".format(result.get("backups")[0].get("backup_name"))
        )
        hosts.all.zos_data_set(
            name=result.get("backups")[0].get("backup_name"), state="absent"
        )
        assert (
            result.get("backups")[0].get("original_name").lower()
            == DEFAULT_DATA_SET.lower()
        )
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS" in result.get("stdout", "")
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


# ---------------------------------------------------------------------------- #
#                                 Input DD Tests                                #
# ---------------------------------------------------------------------------- #


def test_input_empty(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content="")),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


def test_input_large(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    contents = ""
    for i in range(50000):
        contents += "this is line {0}\n".format(i)
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=contents)),
        ],
    )
    for result in results.contacted.values():
        # pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 12
        assert len(result.get("dd_names", [])) > 0
        assert len(result.get("dd_names", [{}])[0].get("content")) > 100000
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


def test_input_provided_as_list(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    contents = []
    for i in range(10):
        contents.append(IDCAMS_STDIN)
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                    return_content=dict(type="text"),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=contents)),
        ],
    )
    for result in results.contacted.values():
        # pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert len(result.get("dd_names", [{}])[0].get("content")) > 100
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "return_content_type,expected",
    [
        ("text", "LISTCAT ENTRIES"),
        (
            "base64",
            "@\udcd3\udcc9\udce2\udce3\udcc3\udcc1\udce3@\udcc5\udcd5\udce3\udcd9\udcc9\udcc5",
        ),
    ],
)
def test_input_return_content_types(ansible_zos_module, return_content_type, expected):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                ),
            ),
            dict(
                dd_input=dict(
                    dd_name=SYSIN_DD,
                    content=IDCAMS_STDIN,
                    return_content=dict(type=return_content_type),
                )
            ),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert expected in "\n".join(result.get("dd_names", [{}])[0].get("content"))
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


@pytest.mark.parametrize(
    "src_encoding,response_encoding,expected",
    [
        (
            "iso8859-1",
            "ibm-1047",
            "|\udceeqBFfeF|g\udcefF\udcfdqgB\udcd4\udcd0CBg\udcfdҿ\udcfdqGeFgҿ\udcfd",
        ),
        ("ibm-1047", "iso8859-1", "LISTCAT ENTRIES",),
    ],
)
def test_input_return_text_content_encodings(
    ansible_zos_module, src_encoding, response_encoding, expected
):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_data_set=dict(
                    dd_name=SYSPRINT_DD,
                    data_set_name=DEFAULT_DATA_SET,
                    disposition="new",
                    type="seq",
                ),
            ),
            dict(
                dd_input=dict(
                    dd_name=SYSIN_DD,
                    content=IDCAMS_STDIN,
                    return_content=dict(
                        type="text",
                        src_encoding=src_encoding,
                        response_encoding=response_encoding,
                    ),
                )
            ),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert expected in "\n".join(result.get("dd_names", [{}])[0].get("content"))
    results = hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", False) is True


# ---------------------------------------------------------------------------- #
#                              Unix file DD Tests                              #
# ---------------------------------------------------------------------------- #


def test_failing_path_name(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(dd_unix=dict(dd_name=SYSPRINT_DD, path="1dfa3f4rafwer/f2rfsd",),),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 8
        assert "ValueError" in result.get("msg", "")


def test_create_new_file(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(dd_unix=dict(dd_name=SYSPRINT_DD, path=DEFAULT_PATH_WITH_FILE,),),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.command(cmd="cat {0}".format(DEFAULT_PATH_WITH_FILE))
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS  SYSTEM" in result.get("stdout", "")


def test_write_to_existing_file(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="present")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(dd_unix=dict(dd_name=SYSPRINT_DD, path=DEFAULT_PATH_WITH_FILE,),),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.command(cmd="cat {0}".format(DEFAULT_PATH_WITH_FILE))
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS  SYSTEM" in result.get("stdout", "")


@pytest.mark.parametrize(
    "normal_disposition,expected", [("keep", True), ("delete", False)]
)
def test_file_normal_disposition(ansible_zos_module, normal_disposition, expected):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="present")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    disposition_normal=normal_disposition,
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.stat(path=DEFAULT_PATH_WITH_FILE)
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert result.get("stat", {}).get("exists", not expected) is expected


@pytest.mark.parametrize("mode,expected", [(644, "0644"), (755, "0755")])
def test_file_modes(ansible_zos_module, mode, expected):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD, path=DEFAULT_PATH_WITH_FILE, mode=mode,
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.stat(path=DEFAULT_PATH_WITH_FILE)
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert result.get("stat", {}).get("mode", "") == expected


@pytest.mark.parametrize(
    "access_group,status_group",
    [
        ("rw", ["ocreat", "oexcl"]),
        ("w", ["ocreat", "oexcl"]),
        ("rw", ["ocreat", "oappend"]),
    ],
)
def test_file_path_options(ansible_zos_module, access_group, status_group):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    access_group=access_group,
                    status_group=status_group,
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.command(cmd="cat {0}".format(DEFAULT_PATH_WITH_FILE))
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS  SYSTEM" in result.get("stdout", "")


@pytest.mark.parametrize(
    "block_size", [10, 20, 50, 80, 120],
)
def test_file_block_size(ansible_zos_module, block_size):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    block_size=block_size,
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.command(cmd="cat {0}".format(DEFAULT_PATH_WITH_FILE))
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS  SYSTEM" in result.get("stdout", "")


@pytest.mark.parametrize(
    "record_length", [10, 20, 50, 80, 120],
)
def test_file_record_length(ansible_zos_module, record_length):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    record_length=record_length,
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.command(cmd="cat {0}".format(DEFAULT_PATH_WITH_FILE))
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS  SYSTEM" in result.get("stdout", "")


@pytest.mark.parametrize(
    "record_format", ["u", "vb", "vba", "fb", "fba"],
)
def test_file_record_format(ansible_zos_module, record_format):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    record_format=record_format,
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.command(cmd="cat {0}".format(DEFAULT_PATH_WITH_FILE))
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS  SYSTEM" in result.get("stdout", "")


@pytest.mark.parametrize(
    "return_content_type,expected",
    [
        ("text", "IDCAMS  SYSTEM"),
        (
            "base64",
            "@\udcd3\udcc9\udce2\udce3\udcc3\udcc1\udce3@\udcc5\udcd5\udce3\udcd9\udcc9\udcc5",
        ),
    ],
)
def test_file_return_content(ansible_zos_module, return_content_type, expected):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    return_content=dict(type=return_content_type),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert expected in "\n".join(result.get("dd_names")[0].get("content", []))


@pytest.mark.parametrize(
    "src_encoding,response_encoding,expected",
    [
        ("iso8859-1", "ibm-1047", "qcfe\udcebB||BTBFg\udceb|Bg\udcfdGqfgB"),
        ("ibm-1047", "iso8859-1", "IDCAMS  SYSTEM",),
    ],
)
def test_file_return_text_content_encodings(
    ansible_zos_module, src_encoding, response_encoding, expected
):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_unix=dict(
                    dd_name=SYSPRINT_DD,
                    path=DEFAULT_PATH_WITH_FILE,
                    return_content=dict(
                        type="text",
                        src_encoding=src_encoding,
                        response_encoding=response_encoding,
                    ),
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert expected in "\n".join(result.get("dd_names")[0].get("content", []))


# ---------------------------------------------------------------------------- #
#                                Dummy DD Tests                                #
# ---------------------------------------------------------------------------- #


def test_dummy(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(dd_dummy=dict(dd_name=SYSPRINT_DD,),),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.file(path=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) == 0


# ---------------------------------------------------------------------------- #
#                            Concatenation DD Tests                            #
# ---------------------------------------------------------------------------- #


def test_concatenation_with_data_set_dd_and_response(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_concat=dict(
                    dd_name=SYSPRINT_DD,
                    dds=[
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET,
                                disposition="new",
                                type="seq",
                                return_content=dict(type="text"),
                            )
                        ),
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET_2,
                                disposition="new",
                                type="seq",
                            )
                        ),
                    ],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results2 = hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))
    for result in results2.contacted.values():
        assert result.get("changed") is True


def test_concatenation_with_data_set_dd_with_replace_and_backup(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="present", type="seq")
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="present", type="seq")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_concat=dict(
                    dd_name=SYSPRINT_DD,
                    dds=[
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET,
                                disposition="new",
                                type="seq",
                                replace=True,
                                backup=True,
                                return_content=dict(type="text"),
                            )
                        ),
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET_2,
                                disposition="new",
                                type="seq",
                                replace=True,
                                backup=True,
                            )
                        ),
                    ],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")

    for result in results.contacted.values():
        pprint(result)
        hosts.all.zos_data_set(
            name=result.get("backups")[0].get("backup_name"), state="absent"
        )
        hosts.all.zos_data_set(
            name=result.get("backups")[1].get("backup_name"), state="absent"
        )
        assert (
            result.get("backups")[0].get("original_name").lower()
            == DEFAULT_DATA_SET.lower()
        )
        assert (
            result.get("backups")[1].get("original_name").lower()
            == DEFAULT_DATA_SET_2.lower()
        )
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))


def test_concatenation_with_data_set_member(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="present", type="pds")
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_concat=dict(
                    dd_name=SYSPRINT_DD,
                    dds=[
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET_WITH_MEMBER,
                                return_content=dict(type="text"),
                            )
                        ),
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET_2,
                                disposition="new",
                                type="seq",
                            )
                        ),
                    ],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    results2 = hosts.all.shell(
        cmd="cat \"//'{0}'\"".format(DEFAULT_DATA_SET_WITH_MEMBER)
    )
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results3 = hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))
    for result in results2.contacted.values():
        pprint(result)
        assert "IDCAMS" in result.get("stdout", "")
    for result in results3.contacted.values():
        pprint(result)
        assert result.get("changed") is True


def test_concatenation_with_unix_dd_and_response(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_concat=dict(
                    dd_name=SYSPRINT_DD,
                    dds=[
                        dict(
                            dd_unix=dict(
                                path=DEFAULT_PATH_WITH_FILE,
                                return_content=dict(type="text"),
                            )
                        ),
                        dict(
                            dd_data_set=dict(
                                data_set_name=DEFAULT_DATA_SET_2,
                                disposition="new",
                                type="seq",
                            )
                        ),
                    ],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.file(name=DEFAULT_PATH, state="absent")
    results2 = hosts.all.zos_data_set(name=DEFAULT_DATA_SET_2, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))
    for result in results2.contacted.values():
        assert result.get("changed") is True


def test_concatenation_with_unix_dd_and_response(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_concat=dict(
                    dd_name=SYSPRINT_DD,
                    dds=[
                        dict(
                            dd_unix=dict(
                                path=DEFAULT_PATH_WITH_FILE,
                                return_content=dict(type="text"),
                            )
                        ),
                        dict(
                            dd_input=dict(
                                content="Hello world!",
                                return_content=dict(type="text"),
                            )
                        ),
                    ],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    hosts.all.file(name=DEFAULT_PATH, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 1
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))
        assert "Hello world!" in "\n".join(result.get("dd_names")[1].get("content", []))


def test_concatenation_fail_with_unsupported_dd_type(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_concat=dict(
                    dd_name=SYSPRINT_DD,
                    dds=[
                        dict(
                            dd_dummy=dict(
                                path=DEFAULT_PATH_WITH_FILE,
                                return_content=dict(type="text"),
                            ),
                            dd_concat=dict(),
                        ),
                    ],
                ),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == -1
        assert "Unsupported parameters" in result.get("msg", "")


@pytest.mark.parametrize(
    "dds,input_pos,input_content",
    [
        (
            [
                dict(
                    dd_concat=dict(
                        dd_name=SYSPRINT_DD,
                        dds=[
                            dict(
                                dd_unix=dict(
                                    path=DEFAULT_PATH_WITH_FILE,
                                    return_content=dict(type="text"),
                                )
                            ),
                            dict(
                                dd_data_set=dict(
                                    data_set_name=DEFAULT_DATA_SET,
                                    disposition="shr",
                                    return_content=dict(type="text"),
                                )
                            ),
                            dict(
                                dd_input=dict(
                                    content="Hello world!",
                                    return_content=dict(type="text"),
                                )
                            ),
                        ],
                    ),
                ),
                dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
            ],
            2,
            "Hello world!",
        ),
        (
            [
                dict(
                    dd_concat=dict(
                        dd_name=SYSPRINT_DD,
                        dds=[
                            dict(
                                dd_data_set=dict(
                                    data_set_name=DEFAULT_DATA_SET,
                                    disposition="shr",
                                    return_content=dict(type="text"),
                                )
                            ),
                            dict(
                                dd_unix=dict(
                                    path=DEFAULT_PATH_WITH_FILE,
                                    return_content=dict(type="text"),
                                )
                            ),
                            dict(
                                dd_input=dict(
                                    content="Hello world!",
                                    return_content=dict(type="text"),
                                )
                            ),
                        ],
                    ),
                ),
                dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
            ],
            2,
            "Hello world!",
        ),
        (
            [
                dict(
                    dd_concat=dict(
                        dd_name=SYSPRINT_DD,
                        dds=[
                            dict(
                                dd_input=dict(
                                    content="Hello world!",
                                    return_content=dict(type="text"),
                                )
                            ),
                            dict(
                                dd_data_set=dict(
                                    data_set_name=DEFAULT_DATA_SET,
                                    disposition="shr",
                                    return_content=dict(type="text"),
                                )
                            ),
                            dict(
                                dd_unix=dict(
                                    path=DEFAULT_PATH_WITH_FILE,
                                    return_content=dict(type="text"),
                                )
                            ),
                        ],
                    ),
                ),
                dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN,)),
            ],
            0,
            "IDCAMS",
        ),
    ],
)
def test_concatenation_all_dd_types(ansible_zos_module, dds, input_pos, input_content):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="present", type="seq")
    hosts.all.file(path=DEFAULT_PATH, state="directory")
    hosts.all.file(path=DEFAULT_PATH_WITH_FILE, state="absent")
    results = hosts.all.zos_mvs_raw(program_name="idcams", auth=True, dds=dds)
    hosts.all.file(name=DEFAULT_PATH, state="absent")
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 2
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))
        assert input_content in "\n".join(
            result.get("dd_names")[input_pos].get("content", [])
        )


# ---------------------------------------------------------------------------- #
#                                Execution Tests                               #
# ---------------------------------------------------------------------------- #


def test_authorized_program_run_unauthorized(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(program_name="idcams", auth=False, dds=[],)
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 8
        assert len(result.get("dd_names", [])) == 0
        assert "BGYSC0236E" in result.get("msg", "")


def test_unauthorized_program_run_authorized(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(program_name="DSPURX00", auth=True, dds=[],)
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 8
        assert len(result.get("dd_names", [])) == 0
        assert "BGYSC0215E" in result.get("msg", "")


def test_authorized_program_run_authorized(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(program_name="idcams", auth=True, dds=[],)
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 16
        assert len(result.get("dd_names", [])) == 0
        assert "BGYSC0236E" not in result.get("msg", "")


def test_unauthorized_program_run_unauthorized(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(program_name="IEFBR14", auth=False, dds=[],)
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) == 0
        assert "BGYSC0215E" not in result.get("msg", "")


def test_missing_program_name(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(auth=False, dds=[],)
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == -1
        assert len(result.get("dd_names", [])) == 0
        assert "missing required arguments" in result.get("msg", "")


def test_with_parms(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        pgm="iefbr14", auth=False, parm="P1,123,P2=5", dds=[],
    )
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) == 0


def test_with_multiple_of_same_dd_name(ansible_zos_module):
    hosts = ansible_zos_module
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    results = hosts.all.zos_mvs_raw(
        pgm="idcams",
        auth=True,
        dds=[
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    hosts.all.zos_data_set(name=DEFAULT_DATA_SET, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 8
        assert len(result.get("dd_names", [])) == 0
        assert "BGYSC0228E" in result.get("msg", "")


# ---------------------------------------------------------------------------- #
#                                 VIO DD Tests                                 #
# ---------------------------------------------------------------------------- #


def test_vio_as_output(ansible_zos_module):
    hosts = ansible_zos_module
    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(dd_vio=dict(dd_name=SYSPRINT_DD,),),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", 0) == 0
        assert len(result.get("dd_names", [])) == 0


# ---------------------------------------------------------------------------- #
#                                Output DD Tests                               #
# ---------------------------------------------------------------------------- #


def test_output_dd(ansible_zos_module):
    hosts = ansible_zos_module

    results = hosts.all.zos_mvs_raw(
        program_name="idcams",
        auth=True,
        dds=[
            dict(
                dd_output=dict(dd_name=SYSPRINT_DD, return_content=dict(type="text"),),
            ),
            dict(dd_input=dict(dd_name=SYSIN_DD, content=IDCAMS_STDIN)),
        ],
    )
    data_set_name = None
    for result in results.contacted.values():
        pprint(result)
        assert result.get("ret_code", {}).get("code", -1) == 0
        assert len(result.get("dd_names", [])) > 0
        assert "IDCAMS" in "\n".join(result.get("dd_names")[0].get("content", []))
        data_set_name = result.get("dd_names")[0].get("name", "")
        assert data_set_name != ""
    results = hosts.all.zos_data_set(name=data_set_name, state="absent")
    for result in results.contacted.values():
        pprint(result)
        assert result.get("changed", True) is False
