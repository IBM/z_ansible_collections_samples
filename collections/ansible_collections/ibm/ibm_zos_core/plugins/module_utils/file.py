# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

import os
from stat import S_IREAD, S_IWRITE, ST_MODE


def _get_dir_mode(path):
    """Get the mode of an existing directory.
    Defaults to 0600 if directory not found.

    Arguments:
        path {str} -- The absolute path to retrieve directory mode from.

    Returns:
        int -- The mode of the directory.
    """
    mask = S_IREAD | S_IWRITE
    if os.path.isdir(path):
        mask = os.stat(path)[ST_MODE]
    elif os.path.isdir(os.path.dirname(path)):
        mask = os.stat(os.path.dirname(path))[ST_MODE]
    return mask


def make_dirs(path, mode_from=None):
    """Create missing directories for path.
    If path does not end in "/", assumes end of path is
    a file.

    Arguments:
        path {str} -- The path to ensure subdirectories are created for.

    Keyword Arguments:
        mode_from {str} -- Path to existing dir to retrieve the mode from.
        Mode will be used for new directories. (default: {None})
    """
    mode = _get_dir_mode(mode_from) if mode_from is not None else S_IREAD | S_IWRITE
    if path[-1] == "/":
        os.makedirs(path, mode=mode, exist_ok=True)
    else:
        os.makedirs(os.path.dirname(path), mode=mode, exist_ok=True)
    return
