# Copyright (c) IBM Corporation 2019, 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
from ibm_zos_core.tests.helpers.ztest import ZTestHelper
import sys
from mock import MagicMock
import importlib


def pytest_addoption(parser):
    """ Add CLI options and modify optons for pytest-ansible where needed. """
    parser.addoption(
        "--zinventory",
        "-Z",
        action="store",
        default="test_config.yml",
        help="Absolute path to YAML file containing inventory info for functional testing.",
    )


@pytest.fixture(scope="session")
def z_python_interpreter(request):
    """ Generate temporary shell wrapper for python interpreter. """
    path = request.config.getoption("--zinventory")
    helper = ZTestHelper.from_yaml_file(path)
    interpreter_str = helper.build_interpreter_string()
    inventory = helper.get_inventory_info()
    yield (interpreter_str, inventory)


def clean_logs(adhoc):
    """Attempt to clean up logs and messages on the system."""
    # purge logs
    adhoc.all.command(cmd="opercmd '$PJ(*)'")
    # clean up wtor messages
    results = adhoc.all.command(cmd="uname -n")
    system_name = ""
    for result in results.contacted.values():
        system_name = result.get("stdout")
    results = adhoc.all.zos_operator_action_query(system=system_name)
    actions = []
    for result in results.contacted.values():
        actions = result.get("actions", [])
    for action in actions:
        adhoc.all.zos_operator(cmd="{0}cancel".format(action.get("number")))


@pytest.fixture(scope="session")
def ansible_zos_module(request, z_python_interpreter):
    """ Initialize pytest-ansible plugin with values from
    our YAML config and inject interpreter path into inventory. """
    interpreter, inventory = z_python_interpreter
    # next two lines perform similar action to ansible_adhoc fixture
    plugin = request.config.pluginmanager.getplugin("ansible")
    adhoc = plugin.initialize(request.config, request, **inventory)
    # * Inject our environment
    hosts = adhoc["options"]["inventory_manager"]._inventory.hosts
    for host in hosts.values():
        host.vars["ansible_python_interpreter"] = interpreter
        host.vars["ansible_connection"] = "zos_ssh"
    yield adhoc
    try:
        clean_logs(adhoc)
    except Exception:
        pass


# * We no longer edit sys.modules directly to add zoautil_py mock
# * because automatic teardown is not performed, leading to mock pollution
# * across test files.
@pytest.fixture(scope="function")
def zos_import_mocker(mocker):
    """ A wrapper fixture around the pytest-mock mocker fixture.
    Abstracts the requirements for mocking zoautil_py module, zoautil_py needs to be mocked
    in order to import most Ansible modules designed for z/OS use.
    Returns a tuple containing a mocker object and the perform_imports() function.
    The perform_imports() function accepts an import or a list of imports as arguments.
    Arguments should be provided as a string or a list of strings."""
    mocker.patch.dict("sys.modules", zoautil_py=MagicMock())

    def perform_imports(imports):
        """ The perform_imports() function accepts an import or a list of imports as arguments.
        Arguments should be provided as a string or a list of strings.
        Returns the import(s) for use. """
        if type(imports) == str:
            newimp = importlib.import_module(imports)
        elif type(imports) == list:
            newimp = [importlib.import_module(x) for x in imports]
        return newimp

    yield (mocker, perform_imports)
