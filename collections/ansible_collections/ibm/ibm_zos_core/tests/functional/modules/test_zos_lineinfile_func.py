# -*- coding: utf-8 -*-

# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function
from ibm_zos_core.tests.helpers.zos_lineinfile_helper import (
    UssGeneral,
    DsGeneral,
    DsNotSupportedHelper,
)
import os
import sys
import pytest

__metaclass__ = type

TEST_CONTENT = """if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT"""

# supported data set types
DS_TYPE = ['SEQ', 'PDS', 'PDSE']
# not supported data set types
NS_DS_TYPE = ['ESDS', 'RRDS', 'LDS']
ENCODING = ['IBM-1047', 'ISO8859-1', 'UTF-8']

TEST_ENV = dict(
    TEST_CONT=TEST_CONTENT,
    TEST_DIR="/tmp/zos_lineinfile/",
    TEST_FILE="",
    DS_NAME="",
    DS_TYPE="",
    ENCODING="",
)

TEST_INFO = dict(
    test_uss_line_replace=dict(
        path="", regexp="ZOAU_ROOT=", line="ZOAU_ROOT=/mvsutil-develop_dsed",
        state="present"),
    test_uss_line_insertafter_regex=dict(
        insertafter="ZOAU_ROOT=", line="ZOAU_ROOT=/mvsutil-develop_dsed",
        state="present"),
    test_uss_line_insertbefore_regex=dict(
        insertbefore="ZOAU_ROOT=", line="unset ZOAU_ROOT", state="present"),
    test_uss_line_insertafter_eof=dict(
        insertafter="EOF", line="export ZOAU_ROOT", state="present"),
    test_uss_line_insertbefore_bof=dict(
        insertbefore="BOF", line="# this is file is for setting env vars",
        state="present"),
    test_uss_line_replace_match_insertafter_ignore=dict(
        regexp="ZOAU_ROOT=", insertafter="PATH=",
        line="ZOAU_ROOT=/mvsutil-develop_dsed", state="present"),
    test_uss_line_replace_match_insertbefore_ignore=dict(
        regexp="ZOAU_ROOT=", insertbefore="PATH=", line="unset ZOAU_ROOT",
        state="present"),
    test_uss_line_replace_nomatch_insertafter_match=dict(
        regexp="abcxyz", insertafter="ZOAU_ROOT=",
        line="ZOAU_ROOT=/mvsutil-develop_dsed", state="present"),
    test_uss_line_replace_nomatch_insertbefore_match=dict(
        regexp="abcxyz", insertbefore="ZOAU_ROOT=", line="unset ZOAU_ROOT",
        state="present"),
    test_uss_line_replace_nomatch_insertafter_nomatch=dict(
        regexp="abcxyz", insertafter="xyzijk",
        line="ZOAU_ROOT=/mvsutil-develop_dsed", state="present"),
    test_uss_line_replace_nomatch_insertbefore_nomatch=dict(
        regexp="abcxyz", insertbefore="xyzijk", line="unset ZOAU_ROOT",
        state="present"),
    test_uss_line_absent=dict(regexp="ZOAU_ROOT=", line="", state="absent"),
    test_ds_line_replace=dict(test_name="T1"),
    test_ds_line_insertafter_regex=dict(test_name="T2"),
    test_ds_line_insertbefore_regex=dict(test_name="T3"),
    test_ds_line_insertafter_eof=dict(test_name="T4"),
    test_ds_line_insertbefore_bof=dict(test_name="T5"),
    test_ds_line_replace_match_insertafter_ignore=dict(test_name="T6"),
    test_ds_line_replace_match_insertbefore_ignore=dict(test_name="T7"),
    test_ds_line_replace_nomatch_insertafter_match=dict(test_name="T8"),
    test_ds_line_replace_nomatch_insertbefore_match=dict(test_name="T9"),
    test_ds_line_replace_nomatch_insertafter_nomatch=dict(test_name="T10"),
    test_ds_line_replace_nomatch_insertbefore_nomatch=dict(test_name="T11"),
    test_ds_line_absent=dict(test_name="T12"),
    expected=dict(test_uss_line_replace="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/mvsutil-develop_dsed
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_insertafter_regex="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAU_ROOT=/mvsutil-develop_dsed
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_insertbefore_regex="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
unset ZOAU_ROOT
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_insertafter_eof="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT
export ZOAU_ROOT""",
                  test_uss_line_insertbefore_bof="""# this is file is for setting env vars
if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_replace_match_insertafter_ignore="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/mvsutil-develop_dsed
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_replace_match_insertbefore_ignore="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
unset ZOAU_ROOT
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_replace_nomatch_insertafter_match="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAU_ROOT=/mvsutil-develop_dsed
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_replace_nomatch_insertbefore_match="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
unset ZOAU_ROOT
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_replace_nomatch_insertafter_nomatch="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT
ZOAU_ROOT=/mvsutil-develop_dsed""",
                  test_uss_line_replace_nomatch_insertbefore_nomatch="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT=/usr/lpp/zoautil/v100
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT
unset ZOAU_ROOT""",
                  test_uss_line_absent="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT""",
                  test_uss_line_replace_quoted="""if [ -z STEPLIB ] && tty -s;
then
    export STEPLIB=none
    exec -a 0 SHELL
fi
TZ=PST8PDT
export TZ
LANG=C
export LANG
readonly LOGNAME
PATH=/usr/lpp/zoautil/v100/bin:/usr/lpp/rsusr/ported/bin:/bin:/var/bin
export PATH
LIBPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
export LIBPATH
NLSPATH=/usr/lib/nls/msg/%L/%N
export NLSPATH
MANPATH=/usr/man/%L
export MANPATH
MAIL=/usr/mail/LOGNAME
export MAIL
umask 022
ZOAU_ROOT="/mvsutil-develop_dsed"
ZOAUTIL_DIR=/usr/lpp/zoautil/v100
PYTHONPATH=/usr/lpp/izoda/v110/anaconda/lib:/usr/lpp/zoautil/v100/lib:/lib
PKG_CONFIG_PATH=/usr/lpp/izoda/v110/anaconda/lib/pkgconfig
PYTHON_HOME=/usr/lpp/izoda/v110/anaconda
_BPXK_AUTOCVT=ON
export ZOAU_ROOT
export ZOAUTIL_DIR
export ZOAUTIL_DIR
export PYTHONPATH
export PKG_CONFIG_PATH
export PYTHON_HOME
export _BPXK_AUTOCVT"""),
)

#########################
# USS test cases
#########################


@pytest.mark.uss
def test_uss_line_replace(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace", ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace"],
        TEST_INFO["expected"]["test_uss_line_replace"])


@pytest.mark.uss
def test_uss_line_insertafter_regex(ansible_zos_module):
    UssGeneral(
        "test_uss_line_insertafter_regex", ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_insertafter_regex"],
        TEST_INFO["expected"]["test_uss_line_insertafter_regex"])


@pytest.mark.uss
def test_uss_line_insertbefore_regex(ansible_zos_module):
    UssGeneral(
        "test_uss_line_insertbefore_regex", ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_insertbefore_regex"],
        TEST_INFO["expected"]["test_uss_line_insertbefore_regex"])


@pytest.mark.uss
def test_uss_line_insertafter_eof(ansible_zos_module):
    UssGeneral(
        "test_uss_line_insertafter_eof", ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_insertafter_eof"],
        TEST_INFO["expected"]["test_uss_line_insertafter_eof"])


@pytest.mark.uss
def test_uss_line_insertbefore_bof(ansible_zos_module):
    UssGeneral(
        "test_uss_line_insertbefore_bof", ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_insertbefore_bof"],
        TEST_INFO["expected"]["test_uss_line_insertbefore_bof"])


@pytest.mark.uss
def test_uss_line_replace_match_insertafter_ignore(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace_match_insertafter_ignore", ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_replace_match_insertafter_ignore"],
        TEST_INFO["expected"]["test_uss_line_replace_match_insertafter_ignore"]
    )


@pytest.mark.uss
def test_uss_line_replace_match_insertbefore_ignore(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace_match_insertbefore_ignore", ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_replace_match_insertbefore_ignore"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_match_insertbefore_ignore"]
    )


@pytest.mark.uss
def test_uss_line_replace_nomatch_insertafter_match(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace_nomatch_insertafter_match", ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_replace_nomatch_insertafter_match"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertafter_match"]
    )


@pytest.mark.uss
def test_uss_line_replace_nomatch_insertbefore_match(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace_nomatch_insertbefore_match", ansible_zos_module,
        TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertbefore_match"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertbefore_match"]
    )


@pytest.mark.uss
def test_uss_line_replace_nomatch_insertafter_nomatch(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace_nomatch_insertafter_nomatch",
        ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertafter_nomatch"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertafter_nomatch"]
    )


@pytest.mark.uss
def test_uss_line_replace_nomatch_insertbefore_nomatch(ansible_zos_module):
    UssGeneral(
        "test_uss_line_replace_nomatch_insertbefore_nomatch",
        ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertbefore_nomatch"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertbefore_nomatch"]
    )


@pytest.mark.uss
def test_uss_line_absent(ansible_zos_module):
    UssGeneral(
        "test_uss_line_absent", ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_absent"],
        TEST_INFO["expected"]["test_uss_line_absent"])


@pytest.mark.uss
def test_uss_line_replace_quoted_escaped(ansible_zos_module):
    TEST_INFO["test_uss_line_replace"]["line"] = 'ZOAU_ROOT=\"/mvsutil-develop_dsed\"'
    UssGeneral(
        "test_uss_line_replace", ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace"],
        TEST_INFO["expected"]["test_uss_line_replace_quoted"])


@pytest.mark.uss
def test_uss_line_replace_quoted_not_escaped(ansible_zos_module):
    TEST_INFO["test_uss_line_replace"]["line"] = 'ZOAU_ROOT="/mvsutil-develop_dsed"'
    UssGeneral(
        "test_uss_line_replace", ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace"],
        TEST_INFO["expected"]["test_uss_line_replace_quoted"])


#########################
# Dataset test cases
#########################

@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace(ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    TEST_INFO["test_uss_line_replace"]["line"] = 'ZOAU_ROOT=/mvsutil-develop_dsed'
    DsGeneral(
        TEST_INFO["test_ds_line_replace"]["test_name"], ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_replace"],
        TEST_INFO["expected"]["test_uss_line_replace"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_insertafter_regex(ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_insertafter_regex"]["test_name"],
        ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_insertafter_regex"],
        TEST_INFO["expected"]["test_uss_line_insertafter_regex"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_insertbefore_regex(ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_insertbefore_regex"]["test_name"],
        ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_insertbefore_regex"],
        TEST_INFO["expected"]["test_uss_line_insertbefore_regex"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_insertafter_eof(ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_insertafter_eof"]["test_name"],
        ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_insertafter_eof"],
        TEST_INFO["expected"]["test_uss_line_insertafter_eof"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_insertbefore_bof(ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_insertbefore_bof"]["test_name"],
        ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_insertbefore_bof"],
        TEST_INFO["expected"]["test_uss_line_insertbefore_bof"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace_match_insertafter_ignore(
        ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_replace_match_insertafter_ignore"]
        ["test_name"], ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_match_insertafter_ignore"],
        TEST_INFO["expected"]["test_uss_line_replace_match_insertafter_ignore"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace_match_insertbefore_ignore(
        ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_replace_match_insertbefore_ignore"]
        ["test_name"], ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_match_insertbefore_ignore"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_match_insertbefore_ignore"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace_nomatch_insertafter_match(
        ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_replace_nomatch_insertafter_match"]
        ["test_name"], ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertafter_match"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertafter_match"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace_nomatch_insertbefore_match(
        ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_replace_nomatch_insertbefore_match"]
        ["test_name"], ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertbefore_match"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertbefore_match"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace_nomatch_insertafter_nomatch(
        ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_replace_nomatch_insertafter_nomatch"]
        ["test_name"], ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertafter_nomatch"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertafter_nomatch"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_replace_nomatch_insertbefore_nomatch(
        ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_replace_nomatch_insertbefore_nomatch"]
        ["test_name"], ansible_zos_module, TEST_ENV,
        TEST_INFO["test_uss_line_replace_nomatch_insertbefore_nomatch"],
        TEST_INFO["expected"]
        ["test_uss_line_replace_nomatch_insertbefore_nomatch"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", DS_TYPE)
@pytest.mark.parametrize("encoding", ENCODING)
def test_ds_line_absent(ansible_zos_module, dstype, encoding):
    TEST_ENV["DS_TYPE"] = dstype
    TEST_ENV["ENCODING"] = encoding
    DsGeneral(
        TEST_INFO["test_ds_line_absent"]["test_name"], ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_absent"],
        TEST_INFO["expected"]["test_uss_line_absent"]
    )


@pytest.mark.ds
@pytest.mark.parametrize("dstype", NS_DS_TYPE)
def test_ds_not_supported(ansible_zos_module, dstype):
    TEST_ENV["DS_TYPE"] = dstype
    DsNotSupportedHelper(
        TEST_INFO["test_ds_line_replace"]["test_name"], ansible_zos_module,
        TEST_ENV, TEST_INFO["test_uss_line_replace"]
    )
