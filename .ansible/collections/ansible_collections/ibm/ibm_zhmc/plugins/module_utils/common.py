# Copyright 2017-2020 IBM Corp. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Utility functions for use by more than one Ansible module.
"""

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

import logging
import traceback
import platform
import sys

from ansible.module_utils import six

try:
    from zhmcclient import Session
    IMP_ZHMCCLIENT_ERR = None
except ImportError:
    IMP_ZHMCCLIENT_ERR = traceback.format_exc()

try:
    from zhmcclient_mock import FakedSession
    IMP_ZHMCCLIENT_MOCK_ERR = None
except ImportError:
    IMP_ZHMCCLIENT_MOCK_ERR = traceback.format_exc()


class Error(Exception):
    """
    Abstract base class that serves as a common exception superclass for the
    zhmc Ansible module.
    """
    pass


class ParameterError(Error):
    """
    Indicates an error with the module input parameters.
    """
    pass


class StatusError(Error):
    """
    Indicates an error with the status of the partition or LPAR.
    """
    pass


class VersionError(Error):
    """
    Indicates an error with the required version of HMC or SE/CPC.
    """
    pass


# Partition status values that may happen after Partition.start()
START_END_STATUSES = ('active', 'degraded')

# Partition status values that may happen after Partition.stop()
STOP_END_STATUSES = ('stopped', 'terminated', 'paused', 'reservation-error')

# Partition status values that indicate CPC issues
BAD_STATUSES = ('communications-not-active', 'status-check')


# Successful LPAR status values for state=inactive
LPAR_INACTIVE_END_STATUSES = ('not-activated',)

# Successful LPAR status values for state=active
LPAR_ACTIVE_END_STATUSES = ('not-operating', 'operating', 'acceptable',
                            'exceptions')

# Successful LPAR status values for state=loaded
LPAR_LOADED_END_STATUSES = ('operating', 'acceptable', 'exceptions')

# LPAR status values that indicate CPC issues
# TODO: Confirm and then simplify by removing this.
LPAR_BAD_STATUSES = tuple()


def common_fail_on_import_errors(module):
    """
    Check for import errors in this module.
    """
    if IMP_ZHMCCLIENT_ERR is not None:
        module.fail_json(msg=missing_required_lib("zhmcclient"),
                         exception=IMP_ZHMCCLIENT_ERR)
    if IMP_ZHMCCLIENT_MOCK_ERR is not None:
        module.fail_json(msg=missing_required_lib("zhmcclient_mock"),
                         exception=IMP_ZHMCCLIENT_MOCK_ERR)


def missing_required_lib(library, reason=None, url=None):
    """
    Return a message for a missing Python library (= module).
    """
    hostname = platform.node()
    msg = "Failed to import the required Python library " \
          "(%s) on %s's Python %s." % (library, hostname, sys.executable)
    if reason:
        msg += " This is required %s." % reason
    if url:
        msg += " See %s for more info." % url

    msg += (" Please read module documentation and install in the appropriate "
            "location. If the required library is installed, but Ansible is "
            "using the wrong Python interpreter, please consult the "
            "documentation on ansible_python_interpreter")
    return msg


def eq_hex(hex_actual, hex_new, prop_name):
    """
    Test two hex string values of a property for equality.
    """
    if hex_actual:
        try:
            int_actual = int(hex_actual, 16)
        except ValueError:
            raise ParameterError(
                "Unexpected: Actual value of property {0!r} is not a valid  "
                "hex number: {1!r}".format(prop_name, hex_actual))
    else:
        int_actual = None
    if hex_new:
        try:
            int_new = int(hex_new, 16)
        except ValueError:
            raise ParameterError(
                "New value for property {0!r} is not a valid "
                "hex number: {1!r}".format(prop_name, hex_new))
    else:
        int_new = None
    return int_actual == int_new


def _normalized_mac(mac_str):
    mac_ints = [int(h, 16) for h in mac_str.split(':')]
    mac_str = ':'.join(["%02x" % i for i in mac_ints])
    return mac_str


def eq_mac(mac_actual, mac_new, prop_name):
    """
    Test two MAC address string values of a property for equality.
    """

    if mac_actual:
        try:
            mac_actual = _normalized_mac(mac_actual)
        except ValueError:
            raise ParameterError(
                "Unexpected: Actual value of property {0!r} is not a valid "
                "MAC address: {1!r}".format(prop_name, mac_actual))
    else:
        mac_actual = None
    if mac_new:
        try:
            mac_new = _normalized_mac(mac_new)
        except ValueError:
            raise ParameterError(
                "New value for property {0!r} is not a valid "
                "MAC address: {1!r}".format(prop_name, mac_new))
    else:
        mac_new = None
    return mac_actual == mac_new


def get_hmc_auth(hmc_auth):
    """
    Extract HMC userid and password from the 'hmc_auth' module input
    parameter.

    Parameters:
      hmc_auth (dict): value of the 'hmc_auth' module input parameter,
        which is a dictionary with required items 'userid' and 'password'
        and optional items 'ca_certs' and 'verify'.

    Returns:
      tuple(userid, password, ca_certs, verify): A tuple with the respective
        items of the input dictionary. Optional items are defaulted:
        - ca_certs: Defaults to None.
        - verify: Defaults to True.

    Raises:
      ParameterError: An item in the input dictionary was missing.
    """
    try:
        userid = hmc_auth['userid']
    except KeyError:
        raise ParameterError("Required item 'userid' is missing in "
                             "dictionary module parameter 'hmc_auth'.")
    try:
        password = hmc_auth['password']
    except KeyError:
        raise ParameterError("Required item 'password' is missing in "
                             "dictionary module parameter 'hmc_auth'.")
    ca_certs = hmc_auth.get('ca_certs', None)
    verify = hmc_auth.get('verify', True)
    return userid, password, ca_certs, verify


def pull_partition_status(partition):
    """
    Retrieve the partition operational status as fast as possible and return
    it.
    """
    parts = partition.manager.cpc.partitions.list(
        filter_args={'name': partition.name})
    if len(parts) != 1:
        raise AssertionError()
    this_part = parts[0]
    actual_status = this_part.get_property('status')
    return actual_status


def stop_partition(partition, check_mode):
    """
    Ensure that the partition is stopped, by influencing the operational
    status of the partition, regardless of what its current operational status
    is.

    The resulting operational status will be one of STOP_END_STATUSES.

    Parameters:
      partition (zhmcclient.Partition): The partition (must exist, and its
        status property is assumed to be current).
      check_mode (bool): Indicates whether the playbook was run in check mode,
        in which case this method does ot actually stop the partition, but
        just returns what would have been done.

    Returns:
      bool: Indicates whether the partition was changed.

    Raises:
      StatusError: Partition is in one of BAD_STATUSES or did not reach one of
        the STOP_END_STATUSES despite attempting it.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    changed = False
    if not check_mode:
        partition.pull_full_properties()
    status = partition.get_property('status')
    if status in BAD_STATUSES:
        raise StatusError(
            "Target CPC {0!r} has issues; status of partition {1!r} is: {2!r}".
            format(partition.manager.cpc.name, partition.name, status))
    elif status in STOP_END_STATUSES:
        pass
    elif status == 'starting':
        if not check_mode:
            # Let it first finish the starting
            partition.wait_for_status(START_END_STATUSES)
            # Then stop it
            partition.stop()
            status = pull_partition_status(partition)
            if status not in STOP_END_STATUSES:
                raise StatusError(
                    "Could not get partition {0!r} from 'starting' status into "
                    "an inactive status after waiting for its starting to "
                    "complete; current status is: {1!r}".
                    format(partition.name, status))
        else:
            status = 'stopped'
        partition.update_properties_local({'status': status})
        changed = True
    elif status == 'stopping':
        if not check_mode:
            # Let it finish the stopping
            partition.wait_for_status(STOP_END_STATUSES)
            status = pull_partition_status(partition)
            if status not in STOP_END_STATUSES:
                raise StatusError(
                    "Could not get partition {0!r} from 'stopping' status into "
                    "an inactive status after waiting for its stopping to "
                    "complete; current status is: {1!r}".
                    format(partition.name, status))
        else:
            status = 'stopped'
        partition.update_properties_local({'status': status})
        changed = True
    else:
        # status in START_END_STATUSES
        if not check_mode:
            previous_status = pull_partition_status(partition)
            partition.stop()
            status = pull_partition_status(partition)
            if status not in STOP_END_STATUSES:
                raise StatusError(
                    "Could not get partition {0!r} from {1!r} status into "
                    "an inactive status; current status is: {2!r}".
                    format(partition.name, previous_status, status))
        else:
            status = 'stopped'
        partition.update_properties_local({'status': status})
        changed = True
    return changed


def start_partition(partition, check_mode):
    """
    Ensure that the partition is started, by influencing the operational
    status of the partition, regardless of what its current operational status
    is.

    The resulting operational status will be one of START_END_STATUSES.

    Parameters:
      partition (zhmcclient.Partition): The partition (must exist, and its
        status property is assumed to be current).
      check_mode (bool): Indicates whether the playbook was run in check mode,
        in which case this method does not actually change the partition, but
        just returns what would have been done.

    Returns:
      bool: Indicates whether the partition was changed.

    Raises:
      StatusError: Partition is in one of BAD_STATUSES or did not reach one of
        the START_END_STATUSES despite attempting it.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    changed = False
    if not check_mode:
        partition.pull_full_properties()
    status = partition.get_property('status')
    if status in BAD_STATUSES:
        raise StatusError(
            "Target CPC {0!r} has issues; status of partition {1!r} is: {2!r}".
            format(partition.manager.cpc.name, partition.name, status))
    elif status in START_END_STATUSES:
        pass
    elif status == 'stopping':
        if not check_mode:
            # Let it first finish the stopping
            partition.wait_for_status(STOP_END_STATUSES)
            # Then start it
            partition.start()
            status = pull_partition_status(partition)
            if status not in START_END_STATUSES:
                raise StatusError(
                    "Could not get partition {0!r} from 'stopping' status into "
                    "an active status after waiting for its stopping to "
                    "complete; current status is: {1!r}".
                    format(partition.name, status))
        else:
            status = 'active'
        partition.update_properties_local({'status': status})
        changed = True
    elif status == 'starting':
        if not check_mode:
            # Let it finish the starting
            partition.wait_for_status(START_END_STATUSES)
            status = pull_partition_status(partition)
            if status not in START_END_STATUSES:
                raise StatusError(
                    "Could not get partition {0!r} from 'starting' status into "
                    "an active status after waiting for its starting to "
                    "complete; current status is: {1!r}".
                    format(partition.name, status))
        else:
            status = 'active'
        partition.update_properties_local({'status': status})
        changed = True
    else:
        # status in STOP_END_STATUSES
        if not check_mode:
            previous_status = pull_partition_status(partition)
            partition.start()
            status = pull_partition_status(partition)
            if status not in START_END_STATUSES:
                raise StatusError(
                    "Could not get partition {0!r} from {1!r} status into "
                    "an active status; current status is: {2!r}".
                    format(partition.name, previous_status, status))
        else:
            status = 'active'
        partition.update_properties_local({'status': status})
        changed = True
    return changed


def wait_for_transition_completion(partition):
    """
    If the partition is in a transitional state, wait for completion of that
    transition. This is required for updating properties.

    The resulting operational status will be one of START_END_STATUSES or
    STOP_END_STATUSES.

    Parameters:
      partition (zhmcclient.Partition): The partition (must exist, and its
        status property is assumed to be current).

    Raises:
      StatusError: Partition is in one of BAD_STATUSES.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    partition.pull_full_properties()
    status = partition.get_property('status')
    if status in BAD_STATUSES:
        raise StatusError(
            "Target CPC {0!r} has issues; status of partition {1!r} is: {2!r}".
            format(partition.manager.cpc.name, partition.name, status))
    elif status == 'stopping':
        partition.wait_for_status(STOP_END_STATUSES)
    elif status == 'starting':
        partition.wait_for_status(START_END_STATUSES)
    else:
        if not (status in START_END_STATUSES or status in STOP_END_STATUSES):
            raise AssertionError()


def pull_lpar_status(lpar):
    """
    Retrieve the LPAR operational status as fast as possible and return it.
    """
    lpars = lpar.manager.cpc.lpars.list(filter_args={'name': lpar.name})
    if len(lpars) != 1:
        raise AssertionError()
    this_lpar = lpars[0]
    actual_status = this_lpar.get_property('status')
    return actual_status


def ensure_lpar_inactive(logger, lpar, check_mode):
    """
    Ensure that the LPAR is in an inactive status, regardless of what its
    current operational status is.

    If this function returns, the operational status of the LPAR will be one of
    LPAR_INACTIVE_END_STATUSES.

    Parameters:

      logger (logging.Logger): The logger to be used.

      lpar (zhmcclient.Lpar): The LPAR (must exist, and its
        status property is assumed to be current).

      check_mode (bool): Indicates whether the playbook was run in check mode,
        in which case this method does ot actually stop the LPAR, but
        just returns what would have been done.

    Returns:
      bool: Indicates whether the LPAR was changed.

    Raises:
      StatusError: CPC is in one of LPAR_BAD_STATUSES or the LPAR did not reach
        an inactive status despite attempting it.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    changed = False
    status = org_status = pull_lpar_status(lpar)

    if status in LPAR_BAD_STATUSES:
        raise StatusError(
            "Target CPC {0!r} has issues; status of LPAR {1!r} is: {2!r}".
            format(lpar.manager.cpc.name, lpar.name, status))

    if status in LPAR_INACTIVE_END_STATUSES:
        logger.debug("LPAR %r was already inactive with status %r",
                     lpar.name, status)
        return changed

    if not check_mode:
        lpar.deactivate(force=True)
        status = pull_lpar_status(lpar)
    changed = True

    if not check_mode and status not in LPAR_INACTIVE_END_STATUSES:
        raise StatusError(
            "Could not get LPAR {0!r} from {1!r} status into "
            "an inactive status; current status is: {2!r}".
            format(lpar.name, org_status, status))

    return changed


def ensure_lpar_active(
        logger, lpar, check_mode, activation_profile_name, force):
    """
    Ensure that the LPAR is at least active, regardless of what its
    current operational status is.

    If the LPAR has auto-load set, it will continue to become loaded.
    If the LPAR was already loaded, it remains loaded.

    If this function returns, the operational status of the LPAR will be one of
    LPAR_ACTIVE_END_STATUSES.

    Parameters:

      logger (logging.Logger): The logger to be used.

      lpar (zhmcclient.Lpar): The LPAR (must exist, and its
        status property is assumed to be current).

      check_mode (bool): Indicates whether the playbook was run in check mode,
        in which case this method does not actually change the LPAR, but
        just returns what would have been done.

      activation_profile_name (string): The name of the image or load activation
        profile to be used when the LPAR needs to be activated. If None, the
        image or load activation profile specified in the
        'next-activation-profile-name' property is used when the LPAR needs to
        be activated.

        If the LPAR was already active, the `force` parameter determines what
        happens.

      force (bool): Controls what happens when the LPAR was already active:

        If True, the parameters from the specified or defaulted image or load
        activation profile will be applied.

        If False, the parameters from the previously used activation profile
        remain applied and will not be changed. The previously used activation
        profile is shown in the 'last-used-activation-profile' property of the
        LPAR.

        TODO: Verify the statements in the description of the 'force' parameter.

    Returns:
      bool: Indicates whether the LPAR was changed.

    Raises:
      StatusError: CPC is in one of LPAR_BAD_STATUSES or the LPAR did not reach
        an operating status despite attempting it.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    changed = False
    status = org_status = pull_lpar_status(lpar)

    if status in LPAR_BAD_STATUSES:
        raise StatusError(
            "Target CPC {0!r} has issues; status of LPAR {1!r} is: {2!r}".
            format(lpar.manager.cpc.name, lpar.name, status))

    if status in LPAR_ACTIVE_END_STATUSES:
        logger.debug("LPAR %r was already at least active with status %r",
                     lpar.name, status)
        return changed

    if status == 'not-activated':
        if not check_mode:
            lpar.activate(
                activation_profile_name=activation_profile_name, force=False)
            status = pull_lpar_status(lpar)
        changed = True

    if not check_mode and status not in LPAR_ACTIVE_END_STATUSES:
        raise StatusError(
            "Could not get LPAR {0!r} from {1!r} status into "
            "an at least active state; current status is: {2!r}".
            format(lpar.name, org_status, status))

    return changed


def ensure_lpar_loaded(
        logger, lpar, check_mode, activation_profile_name, force):
    """
    Ensure that the LPAR is loaded, regardless of what its current operational
    status is.

    If this function returns, the operational status of the LPAR will be one of
    LPAR_LOADED_END_STATUSES.

    Parameters:

      logger (logging.Logger): The logger to be used.

      lpar (zhmcclient.Lpar): The LPAR (must exist, and its
        status property is assumed to be current).

      check_mode (bool): Indicates whether the playbook was run in check mode,
        in which case this method does not actually change the LPAR, but
        just returns what would have been done.

      activation_profile_name (string): The name of the image or load activation
        profile to be used when the LPAR needs to be activated. If None, the
        image or load activation profile specified in the
        'next-activation-profile-name' property of the LPAR is used when the
        LPAR needs to be activated.

        If the LPAR was already active or loaded, the `force` parameter
        determines what happens.

      force (bool): Controls what happens when the LPAR was already active or
        loaded:

        If True, the parameters from the specified or defaulted image or load
        activation profile will be applied.

        If False, the parameters from the previously used activation profile
        remain applied and will not be changed. The previously used activation
        profile is shown in the 'last-used-activation-profile' property of the
        LPAR.

        TODO: Verify the statements in the description of the 'force' parameter.

    Returns:
      bool: Indicates whether the LPAR was changed.

    Raises:
      StatusError: CPC is in one of LPAR_BAD_STATUSES or the LPAR did not reach
        an operating status despite attempting it.
      zhmcclient.Error: Any zhmcclient exception can happen.
    """
    changed = False
    status = org_status = pull_lpar_status(lpar)

    if status in LPAR_BAD_STATUSES:
        raise StatusError(
            "Target CPC {0!r} has issues; status of LPAR {1!r} is: {2!r}".
            format(lpar.manager.cpc.name, lpar.name, status))

    if status in LPAR_LOADED_END_STATUSES:
        logger.debug("LPAR %r was already loaded with status %r",
                     lpar.name, status)
        return changed

    if status == 'not-activated':
        if not check_mode:
            lpar.activate(
                activation_profile_name=activation_profile_name, force=False)
            status = pull_lpar_status(lpar)
        changed = True

    if status == 'not-operating':
        # The LPAR was defined not to auto-load, so we load it.
        if not check_mode:
            lpar.load()
            status = pull_lpar_status(lpar)
        changed = True

    if not check_mode and status not in LPAR_LOADED_END_STATUSES:
        raise StatusError(
            "Could not get LPAR {0!r} from {1!r} status into "
            "a loaded state; current status is: {2!r}".
            format(lpar.name, org_status, status))

    return changed


def get_session(faked_session, host, userid, password, ca_certs, verify):
    """
    Return a session object for the HMC.

    Parameters:
      faked_session (zhmcclient_mock.FakedSession or None):
        If this object is a `zhmcclient_mock.FakedSession` object, return that
        object.
        Else, return a new `zhmcclient.Session` object from the other arguments.
    """
    if isinstance(faked_session, FakedSession):
        return faked_session
    else:
        verify_cert = ca_certs if verify else False
        return Session(host, userid, password, verify_cert=verify_cert)


def to_unicode(value):
    """
    Return the input value as a unicode string.

    The input value may be and will result in:
    * None -> None
    * binary string -> decoded using UTF-8 to unicode string
    * unicode string -> unchanged
    * list or tuple with items of any of the above -> list with converted items
    """
    if isinstance(value, (list, tuple)):
        list_uval = []
        for val in value:
            uval = to_unicode(val)
            list_uval.append(uval)
        return list_uval
    elif isinstance(value, six.binary_type):
        return value.decode('utf-8')
    elif isinstance(value, six.text_type):
        return value
    elif value is None:
        return None
    else:
        raise TypeError("Value of {0} cannot be converted to unicode: {1!r}".
                        format(type(value), value))


def process_normal_property(
        prop_name, resource_properties, input_props, resource):
    """
    Process a normal (= non-artificial) property.

    Parameters:

      prop_name (string): Property name (using Ansible module names).

      resource_properties (dict): Dictionary of property definitions for the
        resource type (e.g. ZHMC_PARTITION_PROPERTIES). Each value must be a
        tuple (allowed, create, update, update_while_active, eq_func,
        type_cast, required(o), default(o)). For details, see the modules
        using this function. (o) means optional.

      input_props (dict): New properties.

      resource: zhmcclient resource object (e.g. zhmcclient.Partition) with
        all properties pulled.

    Returns:

      tuple of (create_props, update_props, stop), where:
        * create_props: dict of properties for resource creation.
        * update_props: dict of properties for resource update.
        * deactivate (bool): Indicates whether the resource needs to be
          deactivated because there are properties to be updated that
          require that.

    Raises:
      ParameterError: An issue with the module parameters.
    """

    create_props = {}
    update_props = {}
    deactivate = False

    p = resource_properties[prop_name]
    allowed, create, update, update_while_active, eq_func, type_cast = p[0:6]
    # Note: If and when required/default are be used here:
    # try:
    #     required = p[6]
    # except IndexError:
    #     required = False
    # try:
    #     default = p[7]
    # except IndexError:
    #     default = None

    # Double check that the property is not a read-only property
    if not allowed:
        raise AssertionError()
    if not (create or update):
        raise AssertionError()

    hmc_prop_name = prop_name.replace('_', '-')
    input_prop_value = input_props[prop_name]

    if type_cast:
        input_prop_value = type_cast(input_prop_value)

    if resource:
        # Resource does exist.

        current_prop_value = resource.properties.get(hmc_prop_name)

        if eq_func:
            equal = eq_func(current_prop_value, input_prop_value,
                            prop_name)
        else:
            equal = (current_prop_value == input_prop_value)

        if not equal:
            if update:
                update_props[hmc_prop_name] = input_prop_value
                if not update_while_active:
                    deactivate = True
            else:
                raise ParameterError(
                    "Property {0!r} can be set during {1} "
                    "creation but cannot be updated afterwards "
                    "(from {2!r} to {3!r}).".
                    format(prop_name, resource.__class__.__name__,
                           current_prop_value, input_prop_value))
    else:
        # Resource does not exist.
        # Prefer setting the property during resource creation.
        if create:
            create_props[hmc_prop_name] = input_prop_value
        else:
            update_props[hmc_prop_name] = input_prop_value
            if not update_while_active:
                deactivate = True

    return create_props, update_props, deactivate


def log_init(logger_name, log_file=None):
    """
    Set up logging for the loggers of the current Ansible module, and for the
    loggers of the underlying zhmcclient package.

    The log level of these loggers is set to debug.

    If a log file is specified, a log file handler for that log file (with a
    log formatter) is created and attached to these loggers.

    Parameters:

        logger_name (string): Name of the logger to be used for the current
          Ansible module.

        log_file (string): Path name of a log file to log to, or `None`.
          If `None`, logging will be propagated to the Python root logger.
    """

    # The datefmt parameter of logging.Formatter() supports the datetime
    # formatting placeholders of time.strftime(). Unfortunately, the %f
    # placeholder for microseconds is not supported by time.strftime().
    # If datefmt=None, the milliseconds are added manually by the
    # logging.Formatter() class. So this is a choice between precision and
    # indicating the timezone offset.
    # The time is in the local timezone.
    #
    DATEFMT = '%Y-%m-%dT%H:%M:%S%z'  # 2019-02-20T10:54:26+0100
    # DATEFMT = None  # 2019-02-20 10:54:26,123 (= local time)

    if log_file:
        handler = logging.FileHandler(log_file)
        fmt = logging.Formatter(
            fmt='%(asctime)s %(levelname)s %(name)s %(process)d %(message)s',
            datefmt=DATEFMT)
        handler.setFormatter(fmt)
    else:
        handler = None

    logger = logging.getLogger(logger_name)
    logger.setLevel(logging.DEBUG)
    if handler:
        ensure_one_handler(logger, handler)

    logger = logging.getLogger('zhmcclient.hmc')
    logger.setLevel(logging.DEBUG)
    if handler:
        ensure_one_handler(logger, handler)


def ensure_one_handler(logger, handler):
    """
    Ensure that the logger has the specified handler exactly once. The handler
    must be a FileHandler, and the handler is recognized by the file name it
    logs to (i.e. the new and existing handler may be different Python objects).
    """
    for hdlr in logger.handlers:
        if isinstance(hdlr, logging.FileHandler) and \
                hdlr.stream.name == handler.stream.name:
            break
    else:
        logger.addHandler(handler)
