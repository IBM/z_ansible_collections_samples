# Unit testing ServiceNow Ansible Collection

Unit tests in the ServiceNow Ansible collection serve two purposes:

 1. they help us detect backward-incompatible changes in public API, and
 2. serve as a first consumer of our internal APIs.

Each kind of test has its own specifics that we will describe next. But in
general, each test file should have the following structure:

    # -*- coding: utf-8 -*-
    # Copyright: (c) 2021, Copyright Holder <email>
    #
    # GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

    from __future__ import absolute_import, division, print_function

    __metaclass__ = type

    import sys

    import pytest

    from ansible_collections.servicenow.itsm.plugins.modules import my_mod

    pytestmark = pytest.mark.skipif(
        sys.version_info < (2, 7), reason="requires python2.7 or higher"
    )


    class TestMain:
        @pytest.mark.parametrize(
            "params",
            [dict(first="set", of="params"), dict(second="set", of="params")],
        )
        def test_minimal_set_of_params(self, run_main, params):
            success, result = run_main(my_mod, params)

            assert success is True

        def test_fail(self, run_main):
            success, result = run_main(my_mod)

            assert success is False
            assert "instance" in result["msg"]

        # More test cases here


    class TestRun:
        def test_run_state_absent(self, create_module, table_client):
            module = create_module(
                params=dict(
                    instance=dict(host="my.host.name", username="user", password="pass"),
                    state="new",
                )
            )
            # Configure the client mock as appropriate
            table_client.get_record.return_value = {"record": "data"}

            changed, response, diff = my_mod.run(module, client)

            assert changed is False
            assert response == {}

        # More test cases here


    # More test classes here


## Testing module's public API

Unit testing module's public API can be a bit awkward because we need to mock
quite a few things before we can safely test the parameter validation. To make
things a bit more pleasant to use, we created a `run_main` pytest fixture that
abstracts away most of the cruft.

The fixture takes two parameters: a Python module that contains the `main`
method, and a dictionary with parameters to test it with. The `run_main`
fixture then mocks the Ansible internals and our module's `run` function,
executes the code inside the main method, and reports back success indicator
and a result.

The success indicator tells us if the module terminated its run with
`exit_json` or `fail_json`. The result is a dictionary of all keyword arguments
`exit_json` or `fail_json` calls received. The result is useless when the
module terminates as expected because it contains data from the mocked `run`
method, but when we expect a failure, we can use those to check for certain
error messages.

Things we should test using unit tests are:

 1. A minimal set (or sets if there is more than one) of arguments that module
    can receive. This ensures we get notified if we add a new required argument
    to the API (this is a breaking change and requires major version bump).
 2. A set of all valid parameters (again, there can be more than one such set).
    This test makes sure we are notified when we are removing parameters from
    the API (again, this is an API break).
 3. Parameter dependencies such as conditional requirements, mutually exclusive
    parameters, etc. This is where we expect `fail_json` to be called and where
    result inspection might be useful.

For an example setup, see template at the start of the document, or take a look
at existing tests.


## Testing methods that require an `AnsibleModule` instance

At least some of our methods will receive an `AnsibleModule` instance as a
parameter. And to make our lives a bit more bearable, we can use the
`create_module` fixture to create `AnsibleModule` mock instances.

The `create_module` fixture takes two parameters:

 1. `params` should contain parameters as they are after Ansible finishes the
    parsing and validation. By default, instance has no parameters set (empty
    dictionary).
 2. `check_mode` can be either set to `True` or `False` (by default set to
    `False`).

Apart from providing a convenient way of specifying module parameters, the
`create_module` fixture also makes sure our code does not use arbitrary methods
and raises an error in such scenarios. For example, it is an error to call
`fail_json` in a function other than `main`. The only methods and attributes
mock allows us to access are: params, check_mode, warn(), and deprecate().

Again, for an example setup, see template at the start of the document, or take
a look at existing tests.


## Testing methods that require an HTTP client instance

If we need to mock HTTP client responses or make assertions about the client's
methods that were called (and their arguments), we can use the `client` and
`table_client` fixtures. The fixtures return a `Mock` instance with the same
spec as the client class they substitute.
