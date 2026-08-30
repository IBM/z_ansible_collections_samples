# Copyright (c) IBM Corporation 2023, 2024
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from __future__ import absolute_import, division, print_function

__metaclass__ = type

"""
Class implemented for common validations that are not specific to z/OS but rather system or
security related.

"""
import os


def validate_safe_path(path):
    """This function is implemented to validate against path traversal attack
    when using os.path.join function.

    In this action plugin, path is on the controller.

    Parameters
    ----------
    path : str
        A file's path.

    Returns
    -------
    str
        The introduced path.

    Raises
    ------
    DirectoryTraversalError
        User does not have access to a directory.
    """
    if not os.path.isabs(path):
        real_path = os.path.realpath(path)
        if not os.path.exists(real_path) and not real_path.endswith(os.sep):
            # if path doesn't exist and does not contain separator then is likely a member.
            return path
        if not os.access(path=real_path, mode=os.F_OK):
            raise DirectoryTraversalError(real_path)
    return path


class DirectoryTraversalError(Exception):
    """User does not have access to a directory.

    Parameters
    ----------
    path : str
        Directory path.

    Attributes
    ----------
    msg : str
        Human readable string describing the exception.
    """
    def __init__(self, path):
        self.msg = "Detected directory traversal, user does not have access to {0}".format(path)
        super().__init__(self.msg)
