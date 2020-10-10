# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function
__metaclass__ = type

from platform import platform
from os import name as OS_NAME
from sys import platform as SYS_PLATFORM
from subprocess import Popen, PIPE
from ansible.module_utils.six import binary_type, text_type, PY2, PY3
from ansible.module_utils._text import to_text, to_bytes
from shlex import split


NIX_PLATFORMS = frozenset({
    "linux",
    "linux2",
    "darwin",
    "freebsd",
    "openbsd",
    "sunos",
    "netbsd"
})


def is_posix():
    """ Determine if the system is POSIX certified or compliant

    Returns:
        bool -- Whether the system is POSIX
    """
    return OS_NAME == "posix"


def is_nix():
    """ Determine if the system is a variant of Unix, supported by Python.

    Returns:
        bool -- Whether the system is Unix-based
    """
    if not is_posix():
        return False
    system_platform = platform().lower()
    for p_name in NIX_PLATFORMS:
        if p_name in system_platform:
            return True
    return False


def is_win():
    """ Determine if the system is a Windows platform

    Returns:
        bool -- Whether the system is Windows
    """
    return "win32" in platform().lower() or OS_NAME == "nt"


def is_zos():
    """ Determine if the system is a z/OS distribution

    Returns:
        bool -- Whether the system is z/OS
    """
    is_zos_unix = is_posix() and not is_nix()
    return is_zos_unix and SYS_PLATFORM == "zos"


def run_command(args, stdin=None, **kwargs):
    """ Execute a shell command on the current system. This function should only
    be used when AnsibleModule.run_command() is not available. This function
    essentially serves as a wrapper for Python subprocess.Popen and supports all
    of the arguments supported by Popen.

    Required arguments:
        args: args should be a sequence of program arguments or else a single
        string or path-like object. By default, the program to execute is the
        first item in args if args is a sequence. It is recommended to pass
        args as a sequence.

        Refer to the following link for a more detailed description of this
        parameter and other parameters.
        https://docs.python.org/3/library/subprocess.html#subprocess.Popen

    Returns:
        tuple[int, str, str]: The return code, stdout and stderr produced after
        executing the command.
    """
    rc = out = err = None
    if not isinstance(args, (list, binary_type, text_type)):
        rc = -1
        err = "'args' must be list or string"
        return rc, out, err

    if isinstance(args, (text_type, str)):
        if PY2:
            args = to_bytes(args, errors='surrogate_or_strict')
        elif PY3:
            args = to_text(args, errors='surrogateescape')
        args = split(args)

    kwargs.update(
        dict(
            stdin=PIPE if stdin else None,
            stderr=PIPE,
            stdout=PIPE
        )
    )
    try:
        cmd = Popen(args, **kwargs)
    except TypeError as proc_err:
        rc = -1
        err = str(proc_err)
        return rc, out, err

    out, err = tuple(map(to_text, cmd.communicate()))
    rc = cmd.returncode
    return rc, out, err
