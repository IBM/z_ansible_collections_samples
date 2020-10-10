# -*- coding: utf-8 -*-

from __future__ import absolute_import, division, print_function

import os
import shutil
import tempfile

__metaclass__ = type


DUMMY_DATA = """DUMMY DATA ---- LINE 001 ------
DUMMY DATA ---- LINE 002 ------
DUMMY DATA ---- LINE 003 ------
DUMMY DATA ---- LINE 004 ------
DUMMY DATA ---- LINE 005 ------
DUMMY DATA ---- LINE 006 ------
DUMMY DATA ---- LINE 007 ------
"""

# SHELL_EXECUTABLE = "/usr/lpp/rsusr/ported/bin/bash"
SHELL_EXECUTABLE = "/bin/sh"


def populate_dir(dir_path):
    for i in range(5):
        with open(dir_path + "/" + "file" + str(i + 1), "w") as infile:
            infile.write(DUMMY_DATA)


def create_vsam_ksds(ds_name, ansible_zos_module):
    hosts = ansible_zos_module
    alloc_cmd = """     DEFINE CLUSTER (NAME({0})  -
    INDEXED                 -
    RECSZ(80,80)            -
    TRACKS(1,1)             -
    KEYS(5,0)               -
    CISZ(4096)              -
    VOLUMES(000000)         -
    FREESPACE(3,3) )        -
    DATA (NAME({0}.DATA))   -
    INDEX (NAME({0}.INDEX))""".format(
        ds_name
    )

    return hosts.all.shell(
        cmd="mvscmdauth --pgm=idcams --sysprint=* --sysin=stdin",
        executable=SHELL_EXECUTABLE,
        stdin=alloc_cmd,
    )


def test_copy_local_file_to_non_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="absent")
        copy_res = hosts.all.zos_copy(src="/etc/profile", dest=dest_path)
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_local_file_to_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="touch")
        stat_res = list(hosts.all.stat(path=dest_path).contacted.values())
        timestamp = stat_res[0].get("stat").get("atime")
        assert timestamp is not None
        copy_res = hosts.all.zos_copy(src="/etc/profile", dest=dest_path)
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_local_file_to_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/testdir"
    try:
        hosts.all.file(path=dest_path, state="directory")
        copy_res = hosts.all.zos_copy(src="/etc/profile", dest=dest_path)
        stat_res = hosts.all.stat(path=dest_path + "/" + "profile")
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_local_file_to_non_existing_sequential_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.SEQ.FUNCTEST"
    src_file = "/etc/profile"
    try:
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_existing_sequential_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.SEQ.FUNCTEST"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
            assert v_cp.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDS.FUNCTEST"
    dest_path = "USER.TEST.PDS.FUNCTEST(DATA)"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest_path, type="MEMBER", replace="yes")
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest_path)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest_path),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_non_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDS.FUNCTEST"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest + "(PROFILE)"),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_non_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDS.FUNCTEST"
    dest_path = "USER.TEST.PDS.FUNCTEST(PROFILE)"
    src_file = "/etc/profile"
    try:
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest_path)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest_path),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_dir_to_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    source_path = tempfile.mkdtemp()
    dest = "USER.TEST.PDS.FUNCTEST"
    try:
        populate_dir(source_path)
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest + "(FILE1)", type="MEMBER", replace="yes")
        copy_result = hosts.all.zos_copy(src=source_path, dest=dest)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest + "(FILE2)"),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        shutil.rmtree(source_path)
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_dir_to_non_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    source_path = tempfile.mkdtemp()
    dest = "USER.TEST.PDS.FUNCTEST"
    try:
        populate_dir(source_path)
        copy_result = hosts.all.zos_copy(src=source_path, dest=dest)
        verify_copy = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\" > /dev/null 2>/dev/null".format(dest),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        shutil.rmtree(source_path)
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_uss_binary(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="absent")
        copy_res = hosts.all.zos_copy(
            src="/etc/profile", dest=dest_path, is_binary=True
        )
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_local_file_to_sequential_data_set_binary(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.SEQ.FUNCTEST"
    src_file = "/etc/profile"
    try:
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest, is_binary=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_pds_member_binary(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDS.FUNCTEST"
    dest_path = "USER.TEST.PDS.FUNCTEST(DATA)"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest_path, type="MEMBER", replace="yes")
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest_path, is_binary=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest_path),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_local_file_to_pdse_member_binary(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDS.FUNCTEST"
    dest_path = "USER.TEST.PDS.FUNCTEST(DATA)"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest_path, type="MEMBER", replace="yes")
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest_path, is_binary=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest_path),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_uss_file_to_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    remote_src = "/etc/profile"
    dest = "/tmp/test_profile"
    try:
        hosts.all.file(path=dest, state="absent")
        copy_result = hosts.all.zos_copy(src=remote_src, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for st in stat_res.contacted.values():
            assert st.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_uss_file_to_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    remote_src = "/etc/profile"
    dest = "/tmp"
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="absent")
        copy_result = hosts.all.zos_copy(src=remote_src, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest_path)
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for st in stat_res.contacted.values():
            assert st.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_uss_file_to_non_existing_sequential_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.SEQ.FUNCTEST"
    src_file = "/etc/profile"
    try:
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_uss_file_to_existing_sequential_data_set(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.SEQ.FUNCTEST"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_uss_file_to_non_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDSE.FUNCTEST"
    dest_path = "USER.TEST.PDSE.FUNCTEST(DATA)"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest_path, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest_path),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_uss_file_to_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "USER.TEST.PDSE.FUNCTEST"
    dest_path = "USER.TEST.PDSE.FUNCTEST(DATA)"
    src_file = "/etc/profile"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest_path, type="MEMBER", replace="yes")
        copy_result = hosts.all.zos_copy(src=src_file, dest=dest_path, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest_path),
            executable=SHELL_EXECUTABLE,
        )
        for cp_res in copy_result.contacted.values():
            assert cp_res.get("msg") is None
        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_uss_dir_to_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    src_dir = "/tmp/testdir"
    dest = "USER.TEST.PDSE.FUNCTEST"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.file(path=src_dir, state="directory")
        for i in range(5):
            hosts.all.file(path=src_dir + "/" + "file" + str(i), state="touch")

        copy_res = hosts.all.zos_copy(src=src_dir, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest + "(FILE2)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
    finally:
        hosts.all.file(path=src_dir, state="absent")
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_uss_dir_to_non_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    src_dir = "/tmp/testdir"
    dest = "USER.TEST.PDSE.FUNCTEST"
    try:
        hosts.all.file(path=src_dir, state="directory")
        for i in range(5):
            hosts.all.file(path=src_dir + "/" + "file" + str(i), state="touch")

        copy_res = hosts.all.zos_copy(src=src_dir, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\" > /dev/null 2>/dev/null".format(dest + "(FILE2)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
    finally:
        hosts.all.file(path=src_dir, state="absent")
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_ps_to_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest = "/tmp/ddchkpt"
    try:
        hosts.all.file(path=dest, state="touch")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        verify_copy = hosts.all.shell(
            cmd="cat {0}".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_ps_to_non_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest = "/tmp/ddchkpt"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        verify_copy = hosts.all.shell(
            cmd="cat {0}".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_ps_to_existing_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest = "/tmp/ddchkpt"
    dest_path = dest + "/" + "IMSTESTL.IMS01.DDCHKPT"
    try:
        hosts.all.file(path=dest, state="directory")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest_path)
        verify_copy = hosts.all.shell(
            cmd="cat {0}".format(dest_path), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_ps_to_non_existing_ps(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest = "USER.TEST.SEQ.FUNCTEST"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_ps_to_existing_ps(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest = "USER.TEST.SEQ.FUNCTEST"
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_ps_to_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest_ds = "USER.TEST.PDSE.FUNCTEST"
    dest = dest_ds + "(DATA)"
    try:
        hosts.all.zos_data_set(name=dest_ds, type="pdse", state="present")
        hosts.all.zos_data_set(name=dest, type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_ps_to_non_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.IMS01.DDCHKPT"
    dest_ds = "USER.TEST.PDSE.FUNCTEST"
    dest = dest_ds + "(DATA)"
    try:
        hosts.all.zos_data_set(name=dest_ds, type="pdse", state="present")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pds_to_non_existing_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC"
    dest = "/tmp/"
    dest_path = "/tmp/IMSTESTL.COMNUC"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
            assert result.get("stat").get("isdir") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_pds_to_existing_pds(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC"
    dest = "USER.TEST.PDS.FUNCTEST"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest + "(ATRQUERY)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pds_to_non_existing_pds(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC"
    dest = "USER.TEST.PDS.FUNCTEST"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest + "(ATRQUERY)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pds_to_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC"
    dest = "USER.TEST.PDSE.FUNCTEST"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="cat \"//'{0}'\"".format(dest + "(ATRQUERY)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pdse_to_non_existing_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE"
    dest = "/tmp/"
    dest_path = "/tmp/SYS1.NFSLIBE"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
            assert result.get("stat").get("isdir") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_pdse_to_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE"
    dest = "USER.TEST.PDSE.FUNCTEST"
    try:
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest + "(GFSAMAIN)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pdse_to_non_existing_pdse(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE"
    dest = "USER.TEST.PDSE.FUNCTEST"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest + "(GFSAMAIN)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pds_member_to_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest = "/tmp/atrquery"
    try:
        hosts.all.file(path=dest, state="touch")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_pds_member_to_non_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest = "/tmp/atrquery"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_pds_member_to_existing_ps(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest = "USER.TEST.SEQ.FUNCTEST"
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pds_member_to_non_existing_ps(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest = "USER.TEST.SEQ.FUNCTEST"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pds_member_to_existing_pds_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest, type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pds_member_to_non_existing_pds_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pds_member_to_existing_pds(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(ATRQUERY)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest_ds, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pds_member_to_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest, type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pds_member_to_non_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pdse_member_to_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest = "/tmp/gfsamain"
    try:
        hosts.all.file(path=dest, state="touch")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_pdse_member_to_non_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest = "/tmp/gfsamain"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest, state="absent")


def test_copy_pdse_member_to_existing_ps(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest = "USER.TEST.SEQ.FUNCTEST"
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pdse_member_to_non_existing_ps(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest = "USER.TEST.SEQ.FUNCTEST"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_pdse_member_to_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest, type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pdse_member_to_non_existing_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pdse_member_to_existing_pds_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest, type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pdse_member_to_non_existing_pds_member(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest = "USER.TEST.PDS.FUNCTEST(DATA)"
    try:
        hosts.all.zos_data_set(
            name=dest_ds,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_pds_member_to_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.COMNUC(ATRQUERY)"
    dest = "/tmp/"
    dest_path = "/tmp/ATRQUERY"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest_path)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest_path), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_pdse_member_to_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.NFSLIBE(GFSAMAIN)"
    dest = "/tmp/"
    dest_path = "/tmp/GFSAMAIN"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest, remote_src=True)
        stat_res = hosts.all.stat(path=dest_path)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest_path), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_vsam_ksds_to_existing_vsam_ksds(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.STGINDEX"
    dest_ds = "USER.TEST.VSAM.KSDS"
    try:
        create_vsam_ksds(dest_ds, ansible_zos_module)
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest_ds, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(dest_ds), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stderr")
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "VSAM" in result.get("stdout")
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_vsam_ksds_to_non_existing_vsam_ksds(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "SYS1.STGINDEX"
    dest_ds = "USER.TEST.VSAM.KSDS"
    try:
        copy_res = hosts.all.zos_copy(src=src_ds, dest=dest_ds, remote_src=True)
        verify_copy = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(dest_ds), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stderr")
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "VSAM" in result.get("stdout")
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_empty_vsam_fails(ansible_zos_module):
    hosts = ansible_zos_module
    src_ds = "IMSTESTL.LDS01.WADS2"
    dest_ds = "USER.TEST.VSAM.LDS"
    try:
        copy_res = hosts.all.zos_copy(
            src=src_ds, dest=dest_ds, is_vsam=True, remote_src=True
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is not None
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_inline_content_to_existing_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/inline"
    try:
        hosts.all.file(path=dest_path, state="touch")
        copy_res = hosts.all.zos_copy(content="Inline content", dest=dest_path)
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_inline_content_to_uss_dir(ansible_zos_module):
    hosts = ansible_zos_module
    dest = "/tmp/"
    dest_path = "/tmp/inline_copy"
    try:
        copy_res = hosts.all.zos_copy(content="Inline content", dest=dest)
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_inline_content_to_ps(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "USER.TEST.SEQ.FUNCTEST"
    try:
        copy_res = hosts.all.zos_copy(content="Inline content", dest=dest_path)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest_path), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_path, state="absent")


def test_copy_inline_content_to_pds_member(ansible_zos_module):
    hosts = ansible_zos_module
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest_path = "USER.TEST.PDS.FUNCTEST(CONTENT)"
    try:
        hosts.all.zos_data_set(
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(content="Inline content", dest=dest_path)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest_path), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_inline_content_to_pdse_member(ansible_zos_module):
    hosts = ansible_zos_module
    dest_ds = "USER.TEST.PDS.FUNCTEST"
    dest_path = "USER.TEST.PDS.FUNCTEST(CONTENT)"
    try:
        hosts.all.zos_data_set(
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(content="Inline content", dest=dest_path)
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest_path), executable=SHELL_EXECUTABLE
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_ds, state="absent")


def test_copy_to_existing_dest_not_forced(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="touch")
        copy_res = hosts.all.zos_copy(src="/etc/profile", dest=dest_path, force=False)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            assert result.get("note") is not None
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_local_symlink_to_uss_file(ansible_zos_module):
    hosts = ansible_zos_module
    src_lnk = "/tmp/etclnk"
    dest_path = "/tmp/profile"
    try:
        try:
            os.symlink("/etc/profile", src_lnk)
        except FileExistsError:
            pass
        hosts.all.file(path=dest_path, state="touch")
        copy_res = hosts.all.zos_copy(src=src_lnk, dest=dest_path, local_follow=True)
        verify_copy = hosts.all.shell(
            cmd="head {0}".format(dest_path), executable=SHELL_EXECUTABLE
        )
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.file(path=dest_path, state="absent")
        os.remove(src_lnk)


def test_copy_local_file_to_uss_file_convert_encoding(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="absent")
        copy_res = hosts.all.zos_copy(
            src="/etc/profile",
            dest=dest_path,
            encoding={"from": "ISO8859-1", "to": "IBM-1047"},
        )
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_uss_file_to_uss_file_convert_encoding(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        hosts.all.file(path=dest_path, state="absent")
        copy_res = hosts.all.zos_copy(
            src="/etc/profile",
            dest=dest_path,
            encoding={"from": "IBM-1047", "to": "IBM-1047"},
            remote_src=True,
        )
        stat_res = hosts.all.stat(path=dest_path)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_uss_file_to_pds_member_convert_encoding(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest_path = "USER.TEST.PDS.FUNCTEST"
    try:
        hosts.all.zos_data_set(
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        copy_res = hosts.all.zos_copy(
            src=src,
            dest=dest_path,
            remote_src=True,
            encoding={"from": "IBM-1047", "to": "IBM-1047"},
        )
        verify_copy = hosts.all.shell(
            cmd="head \"//'{0}'\"".format(dest_path + "(PROFILE)"),
            executable=SHELL_EXECUTABLE,
        )
        for result in copy_res.contacted.values():
            assert result.get("msg") is None
        for result in verify_copy.contacted.values():
            assert result.get("rc") == 0
            assert result.get("stdout") != ""
    finally:
        hosts.all.zos_data_set(name=dest_path, state="absent")


def test_ensure_tmp_cleanup(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest = "/tmp"
    dest_path = "/tmp/profile"
    try:
        stat_dir = hosts.all.shell(
            cmd="ls -l", executable=SHELL_EXECUTABLE, chdir="/tmp"
        )
        file_count_pre = len(list(stat_dir.contacted.values())[0].get("stdout_lines"))

        copy_res = hosts.all.zos_copy(src=src, dest=dest)
        for result in copy_res.contacted.values():
            assert result.get("msg") is None

        stat_dir = hosts.all.shell(
            cmd="ls -l", executable=SHELL_EXECUTABLE, chdir="/tmp"
        )
        file_count_post = len(list(stat_dir.contacted.values())[0].get("stdout_lines"))
        assert file_count_post == file_count_pre + 1

    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_backup_uss_file_default_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest = "/tmp/profile"
    backup_name = None
    try:
        hosts.all.file(path=dest, state="touch")
        copy_res = hosts.all.zos_copy(src=src, dest=dest, backup=True)

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            backup_name = result.get("backup_name")
            assert backup_name is not None

        stat_res = hosts.all.stat(path=backup_name)
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True

    finally:
        hosts.all.file(path=dest, state="absent")
        if backup_name:
            hosts.all.file(path=backup_name, state="absent")


def test_backup_sequential_data_set_default_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest = "USER.TEST.SEQ.FUNCTEST"
    backup_name = None
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_res = hosts.all.zos_copy(src=src, dest=dest, backup=True)

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            backup_name = result.get("backup_name")
            assert backup_name is not None

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_pds_default_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = tempfile.mkdtemp()
    dest = "USER.TEST.PDS.FUNCTEST"
    backup_name = None
    try:
        populate_dir(src)
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest + "(FILE1)", type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src, dest=dest, backup=True)

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            backup_name = result.get("backup_name")
            assert backup_name is not None

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        shutil.rmtree(src)
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_pdse_default_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = tempfile.mkdtemp()
    dest = "USER.TEST.PDSE.FUNCTEST"
    backup_name = None
    try:
        populate_dir(src)
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest + "(FILE1)", type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(src=src, dest=dest, backup=True)

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            backup_name = result.get("backup_name")
            assert backup_name is not None

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        shutil.rmtree(src)
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_vsam_default_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = "SYS1.STGINDEX"
    dest = "USER.TEST.VSAM.KSDS"
    backup_name = None
    try:
        create_vsam_ksds(dest, ansible_zos_module)
        copy_res = hosts.all.zos_copy(src=src, dest=dest, backup=True, remote_src=True)

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            backup_name = result.get("backup_name")
            assert backup_name is not None

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_uss_file_user_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest = "/tmp/profile"
    backup_name = "/tmp/uss_backup"
    try:
        hosts.all.file(path=dest, state="touch")
        copy_res = hosts.all.zos_copy(
            src=src, dest=dest, backup=True, backup_name=backup_name
        )

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            result.get("backup_name") == backup_name

        stat_res = hosts.all.stat(path=backup_name)
        for result in stat_res.contacted.values():
            assert result.get("stat").get("exists") is True

    finally:
        hosts.all.file(path=dest, state="absent")
        if backup_name:
            hosts.all.file(path=backup_name, state="absent")


def test_backup_sequential_data_set_user_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest = "USER.TEST.SEQ.FUNCTEST"
    backup_name = "USER.TEST.SEQ.FUNCTEST.BACK"
    try:
        hosts.all.zos_data_set(name=dest, type="seq", state="present")
        copy_res = hosts.all.zos_copy(
            src=src, dest=dest, backup=True, backup_name=backup_name
        )

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            result.get("backup_name") == backup_name

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_pds_user_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = tempfile.mkdtemp()
    dest = "USER.TEST.PDS.FUNCTEST"
    backup_name = "USER.TEST.PDS.FUNCTEST.BACK"
    try:
        populate_dir(src)
        hosts.all.zos_data_set(
            name=dest,
            type="pds",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest + "(FILE1)", type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(
            src=src, dest=dest, backup=True, backup_name=backup_name
        )

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            result.get("backup_name") == backup_name

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        shutil.rmtree(src)
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_pdse_user_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = tempfile.mkdtemp()
    dest = "USER.TEST.PDSE.FUNCTEST"
    backup_name = "USER.TEST.PDSE.FUNCTEST.BACK"
    try:
        populate_dir(src)
        hosts.all.zos_data_set(
            name=dest,
            type="pdse",
            space_primary=5,
            space_type="M",
            record_format="fba",
            record_length=25,
        )
        hosts.all.zos_data_set(name=dest + "(FILE1)", type="MEMBER", replace="yes")
        copy_res = hosts.all.zos_copy(
            src=src, dest=dest, backup=True, backup_name=backup_name
        )

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            result.get("backup_name") == backup_name

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        shutil.rmtree(src)
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_backup_vsam_user_backup_path(ansible_zos_module):
    hosts = ansible_zos_module
    src = "SYS1.STGINDEX"
    dest = "USER.TEST.VSAM.KSDS"
    backup_name = "USER.TEST.VSAM.KSDS.BACK"
    try:
        create_vsam_ksds(dest, ansible_zos_module)
        copy_res = hosts.all.zos_copy(
            src=src, dest=dest, backup=True, remote_src=True, backup_name=backup_name
        )

        for result in copy_res.contacted.values():
            assert result.get("msg") is None
            result.get("backup_name") == backup_name

        stat_res = hosts.all.shell(
            cmd="tsocmd \"LISTDS '{0}'\"".format(backup_name),
            executable=SHELL_EXECUTABLE,
        )
        for result in stat_res.contacted.values():
            assert result.get("rc") == 0
            assert "NOT IN CATALOG" not in result.get("stdout")
            assert "NOT IN CATALOG" not in result.get("stderr")

    finally:
        hosts.all.zos_data_set(name=dest, state="absent")
        if backup_name:
            hosts.all.zos_data_set(name=backup_name, state="absent")


def test_copy_local_file_insufficient_read_permission_fails(ansible_zos_module):
    hosts = ansible_zos_module
    src_path = "/tmp/testfile"
    dest = "/tmp"
    try:
        open(src_path, "w").close()
        os.chmod(src_path, 0)
        copy_res = hosts.all.zos_copy(src=src_path, dest=dest)
        for result in copy_res.contacted.values():
            assert result.get("msg") is not None
            assert "read permission" in result.get("msg")
    finally:
        if os.path.exists(src_path):
            os.remove(src_path)


def test_copy_non_existent_local_file_fails(ansible_zos_module):
    hosts = ansible_zos_module
    src_path = "/tmp/non_existent_src"
    dest = "/tmp"

    copy_res = hosts.all.zos_copy(src=src_path, dest=dest)
    for result in copy_res.contacted.values():
        assert result.get("msg") is not None
        assert "does not exist" in result.get("msg")


def test_copy_local_file_to_vsam_fails(ansible_zos_module):
    hosts = ansible_zos_module
    src = "/etc/profile"
    dest = "USER.TEST.VSAM.KSDS"
    try:
        create_vsam_ksds(dest, ansible_zos_module)
        copy_res = hosts.all.zos_copy(src=src, dest=dest)
        for result in copy_res.contacted.values():
            assert result.get("msg") is not None
            assert "Incompatible" in result.get("msg")
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_copy_sequential_data_set_to_vsam_fails(ansible_zos_module):
    hosts = ansible_zos_module
    src = "IMSTESTL.IMS01.DDCHKPT"
    dest = "USER.TEST.VSAM.KSDS"
    try:
        create_vsam_ksds(dest, ansible_zos_module)
        copy_res = hosts.all.zos_copy(src=src, dest=dest, remote_src=True)
        for result in copy_res.contacted.values():
            assert result.get("msg") is not None
            assert "Incompatible" in result.get("msg")
    finally:
        hosts.all.zos_data_set(name=dest, state="absent")


def test_sftp_negative_port_specification_fails(ansible_zos_module):
    hosts = ansible_zos_module
    dest_path = "/tmp/profile"
    try:
        copy_res = hosts.all.zos_copy(src="/etc/profile", dest=dest_path, sftp_port=-1)
        for result in copy_res.contacted.values():
            assert result.get("msg") is not None
    finally:
        hosts.all.file(path=dest_path, state="absent")


def test_copy_multiple_data_set_members(ansible_zos_module):
    hosts = ansible_zos_module
    src = "USER.FUNCTEST.SRC.PDS"
    dest = "USER.FUNCTEST.DEST.PDS"
    member_list = ["MEMBER1", "ABCXYZ", "ABCASD"]
    ds_list = ["{0}({1})".format(src, i) for i in member_list]
    try:
        hosts.all.zos_data_set(name=src, type="pds")
        hosts.all.zos_data_set(name=dest, type="pds")
        hosts.all.zos_data_set(
            batch=[dict(src=i, type="MEMBER", replace="yes") for i in ds_list]
        )

        for i in ds_list:
            hosts.all.zos_copy(content=DUMMY_DATA, dest=i)

        copy_res = hosts.all.zos_copy(src=src + "(ABC*)", dest=dest, remote_src=True)
        for res in copy_res.contacted.values():
            assert res.get("msg") is None

        verify_copy = hosts.all.shell(
            cmd="mls {0}".format(dest), executable=SHELL_EXECUTABLE
        )

        for v_cp in verify_copy.contacted.values():
            assert v_cp.get("rc") == 0
            stdout = v_cp.get("stdout")
            assert stdout is not None
            assert(len(stdout.splitlines())) == 2

    finally:
        hosts.all.zos_data_set(name=dest, state="absent")
        hosts.all.zos_data_set(name=src, state="absent")
