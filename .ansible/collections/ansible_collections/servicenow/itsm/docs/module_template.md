# ServiceNow module template

ServiceNow Ansible Collection modules all have the same high-level structure.
To keep things consistent, use the following template when creating new
modules:

    #!/usr/bin/python
    # -*- coding: utf-8 -*-
    # Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
    #
    # GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

    from __future__ import absolute_import, division, print_function

    __metaclass__ = type

    DOCUMENTATION = r"""
    module: module_name
    author:
      - John Doe (@johndoe)
    short_description: A short description
    description:
      - Longer description that can contain more than one paragraph.
      - Links to relevant upstream sources should also go here. For example,
        like this: U(https://steampunk.si/).
    version_added: 1.0.0
    extends_documentation_fragment:
      - servicenow.itsm.fragment1
      - servicenow.itsm.fragment2
      # More fragments here
    seealso:
      - module: servicenow.itsm.module_name_info
      # More references here
    options:
      check:
        description:
          - The name of the check the entry should match.
          - If left empty a silencing entry will contain an asterisk in the
            check position.
        type: str
        required: true
      # More options here
    """

    EXAMPLES = r"""
    - name: Sample 1 (always use FQCN)
      servicenow.itsm.module_name:
        check: check-disk

    # More examples here
    """

    RETURN = r"""
    record:
      description:
        - Created/updated ServiceNow record.
      returned: success
      type: dict
      sample:
        check: 123
    """

    from ansible.module_utils.basic import AnsibleModule

    from ..module_utils import arguments, client, utils
    # More local imports here (if needed).


    # Add module-specific helpers here


    def run(module, table_client):
        # Implementation here.

        return changed, record, dict(before=a, after=b)


    def main():
        # The first part of main method is dedicated to parsing and validating
        # module parameters.
        module = AnsibleModule(
            supports_check_mode=True,
            argument_spec=dict(
                check=dict(),
            # Other stuff here
        )
        # Any validation that does not require accessing remote also goes
        # here.

        try:
            # HTTP client construction
            snow_client = client.Client(**module.params["instance"])
            table_client = table.TableClient(snow_client)

            changed, record, diff = run(module, table_client)
            module.exit_json(changed=changed, record=record, diff=diff)
        except ServiceNowError as e:
            module.fail_json(msg=str(e))


    if __name__ == "__main__":
        main()

There are a few reasons for this kind of structure, but the main one is ease of
testing. Such structure minimizes the amount of mocking required when unit
testing modules, which is great news since we will unit test a lot once we fix
our API.

We should structure info modules a bit differently since they by definition do
not modify the instance state. The documentation part should still follow the
same structure, but the `run` method and the last part of the `main` method are
a bit simpler:

    def run(module, table_client):
        # Implementation here thar produces a list of records. Most of the
        # time, this will be the `result` part of the API response.

        return records


    def main():
        # The first part of main method is dedicated to parsing and validating
        # module parameters.
        module = AnsibleModule(
            supports_check_mode=True,
            argument_spec=dict(
                check=dict(),
            # Other stuff here
        )
        # Any validation that does not require accessing remote also goes
        # here.

        try:
            # HTTP client construction
            snow_client = client.Client(**module.params["instance"])
            table_client = table.TableClient(snow_client)

            records = run(module, table_client)
            module.exit_json(changed=False, records=records)
        except ServiceNowError as e:
            module.fail_json(msg=str(e))


    if __name__ == "__main__":
        main()

Another difference is in the return value. All info modules must return a list
of records in the `records` return field:

    RETURN = r"""
    records:
      description:
        - Matching ServiceNow records.
      returned: success
      type: list
      elements: dict
      sample:
        - check: 123
    """


## The `main` function

The purpose of the main method is two-fold:
1. __Static validation of module parameters__. This is where we define
   module's public API and enforce the parameter correctness before we
   start talking with the backend services.

   The vast majority of validation comes bundled with the `AnsibleModule`
   constructor, which means we just need to properly define the argument
   specification. But we can also add custom validation that does not require
   access to remote services.

2. __Construction of the HTTP client__. Once the validation is over, the
   `main` method must construct the HTTP and/or table client.

Once the `main` method constructs the Ansible module and validates its
parameters, it needs to create the HTTP client. After that, it
delegates further work to the `run` method, passing it the previously
constructed module and client instances.

An important thing to note is that from parameter validation onwards,
all the code (including HTTP client construction) is wrapped in
a `try: ... except ServiceNowError` block. The `main` method will call
`exit_json` or `fail_json` as appropriate. No other exceptions should be
handled here because they indicate a bug in our code.


## The `run` function

The `run` function is the main workhorse of the module. Its main task
is to delegate the real work to more specialized functions if required.

The `run` function receives two parameters:

1. an `AnsibleModule` instance;
2. a `Client` or `TableClient` instance.

Why do we need a module instance and not just parameters and check mode
boolean? Most of the time, the answer to that question is "we do not". But
there is one area where module instance is still needed: deprecations and
warnings. And while most of those things can be handled in the main method,
some of the (the ones that depend on the state of the backend) cannot.

In practice, we should only access four things on the module parameter:

 1. `module.params` when we need to access the module parameters.
 2. `module.check_mode` when we need to determine if we are running in check
    mode.
 3. `module.warn()` when we need to emit a warning to the user.
 4. `module.deprecate()` for dynamic deprecation messages.

If no error occurs during the module execution, the `run` method should return
three pieces of information:

 1. A boolean indication a change.
 2. A dictionary with the record data.
 3. A diff dictionary, containing states before and after the change.

Info modules are a bit simpler and their `run` method should just return back
the list of records retrieved from the ServiceNow API.

Errors should raise a `ServiceNowError` (or its descendant) with a clear
message about the error. The utility wrapper will catch this exception and
convert it to an appropriate `fail_json` call.
