from __future__ import (absolute_import, division, print_function)
__metaclass__ = type
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import BetterArgParser  # pylint: disable=import-error
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.zos_mvs_raw import MVSCmd  # pylint: disable=import-error
import tempfile
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (  # pylint: disable=import-error
    MissingZOAUImport,
)
import tempfile
import pprint
try:
    from zoautil_py import Datasets, types  # pylint: disable=import-error
except Exception:
    Datasets = MissingZOAUImport()
    types = MissingZOAUImport()


module = None


def _create_temp_data_set(hlq):
    """Create a temporary data set.
    Arguments:
        hlq {str} -- The HLQ to use for the temporary data set's name.
    Returns:
        str -- The name of the temporary data set.
    """
    temp_data_set_name = Datasets.temp_name(hlq)
    _create_data_set(
        temp_data_set_name, {"type": "SEQ", "size": "5M", "format": "FB", "length": 80},
    )
    return temp_data_set_name


def _create_data_set(name, extra_args=None):
    """A wrapper around zoautil_py
    Dataset.create() to raise exceptions on failure.
    Arguments:
        name {str} -- The name of the data set to create.
    Raises:
        DatasetCreateError: When data set creation fails.
    """
    if extra_args is None:
        extra_args = {}
    rc = Datasets.create(name, **extra_args)
    if rc > 0:
        raise DatasetCreateError(name, rc)
    return


def _write_data_set(name, contents):
    """Write text to a data set.
    Arguments:
        name {str} -- The name of the data set.
        contents {str} -- The text to write to the data set.
    Raises:
        DatasetWriteError: When write to the data set fails.
    """
    # rc = Datasets.write(name, contents)
    temp = tempfile.NamedTemporaryFile(delete=False)
    with open(temp.name, "w") as f:
        f.write(contents)
    rc, stdout, stderr = module.run_command(
        "cp -O u {0} \"//'{1}'\"".format(temp.name, name)
    )
    if rc != 0:
        raise DatasetWriteError(name, rc, stderr)
    return


def _delete_data_set(name):
    """A wrapper around zoautil_py
    Dataset.delete() to raise exceptions on failure.
    Arguments:
        name {str} -- The name of the data set to delete.
    Raises:
        DatasetDeleteError: When data set deletion fails.
    """
    rc = Datasets.delete(name)
    if rc > 0:
        raise DatasetDeleteError(name, rc)
    return


class Error(Exception):
    def __init__(self, *args):
        super(Error, self).__init__(*args)


class DatasetWriteError(Error):
    def __init__(self, data_set, rc, message=""):
        self.msg = 'An error occurred during write of data set "{0}". RC={1}. {2}'.format(
            data_set, rc, message
        )
        super(DatasetWriteError, self).__init__(self.msg)


class DatasetDeleteError(Error):
    def __init__(self, data_set, rc):
        self.msg = 'An error occurred during deletion of data set "{0}". RC={1}'.format(
            data_set, rc
        )
        super(DatasetDeleteError, self).__init__(self.msg)


class DatasetCreateError(Error):
    def __init__(self, data_set, rc):
        self.msg = 'An error occurred during creation of data set "{0}". RC={1}'.format(
            data_set, rc
        )
        super(DatasetCreateError, self).__init__(self.msg)
