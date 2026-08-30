
# Copyright (c) 2019, 2020 Blake Becker <blake.becker@ibm.com>
# Copyright (c) IBM Corporation 2020
# LICENSE: [GNU General Public License version 3](https://opensource.org/licenses/GPL-3.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type
import pytest
from ibm_zos_ims.tests.helpers.ztest import ZTestHelper
import sys
from mock import MagicMock
import importlib


def pytest_addoption(parser):
    """ Add CLI options and modify optons for pytest-ansible where needed. """
    parser.addoption("--zinventory", "-Z", action="store", default="test_config.yml",
                     help="Absolute path to YAML file containing inventory info for functional testing.")


@pytest.fixture(scope="session")
def z_python_interpreter(request):
    """ Generate temporary shell wrapper for python interpreter. """
    path = request.config.getoption("--zinventory")
    helper = ZTestHelper.from_yaml_file(path)
    interpreter_str = helper.build_interpreter_string()
    inventory = helper.get_inventory_info()
    ims_vars = helper.build_ims_dict()
    yield (interpreter_str, inventory, ims_vars)


@pytest.fixture(scope='function')
def ansible_zos_module(request, z_python_interpreter):
    """ Initialize pytest-ansible plugin with values from
    our YAML config and inject interpreter path into inventory. """
    interpreter, inventory, ims_variables = z_python_interpreter
    # next two lines perform similar action to ansible_adhoc fixture
    plugin = request.config.pluginmanager.getplugin("ansible")
    adhoc = plugin.initialize(request.config, request, **inventory)
    # * Inject our environment
    hosts = adhoc['options']['inventory_manager']._inventory.hosts
    for host in hosts.values():
        host.vars['ansible_python_interpreter'] = interpreter
        host.vars['STEPLIB'] = ims_variables["STEPLIB"]
        host.vars['JOB_CARD'] = ims_variables["JOB_CARD"]
    yield adhoc


# * We no longer edit sys.modules directly to add zoautil_py mock
# * because automatic teardown is not performed, leading to mock pollution
# * across test files.
@pytest.fixture(scope='function')
def zos_import_mocker(mocker):
    """ A wrapper fixture around the pytest-mock mocker fixture.
    Abstracts the requirements for mocking zoautil_py module, zoautil_py needs to be mocked
    in order to import most Ansible modules designed for z/OS use.
    Returns a tuple containing a mocker object and the perform_imports() function.
    The perform_imports() function accepts an import or a list of imports as arguments.
    Arguments should be provided as a string or a list of strings."""
    mocker.patch.dict('sys.modules', zoautil_py=MagicMock())

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
