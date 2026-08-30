# Integration testing ServiceNow Ansible Collection

With the help of integration tests, we make sure that the plugins from the
ServiceNow Ansible Collection work as expected when we run them against
a ServiceNow instance. We use `ansible-test integration`, which executes
our test targets (roles) that contain test playbooks.

## Prerequisites

A prerequisite for running integration tests is an active ServiceNow
[Personal Developer Instance](pdi) (PDI). You can request your PDI from 
ServiceNow as described in [the official ServiceNow guide](pdi-guide).

Once your PDI is up and running, you will need to make note of the access
credentials for your instance. Copy the contents of the file
`tests/integration/integration_config.yml.template` to
`tests/integration/integration_config.yml`:

    (venv) $ cp tests/integration/integration_config.yml.template tests/integration/integration_config.yml

Then, open `integration_config.yml` for editing and substitute the placeholder
variable values with  with connection and access details for your PDI.

[pdi]:
    https://developer.servicenow.com/dev.do#!/guides/quebec/developer-program/pdi-guide/personal-developer-instance-guide-introduction
    (Introduction to Personal Developer Instances)
[pdi-guide]:
    https://developer.servicenow.com/dev.do#!/guides/quebec/developer-program/pdi-guide/obtaining-a-pdi
    (Instructions on obtaining a Personal Developer Instance)

## Running integration tests

Now you are ready to run integration tests against your PDI:

* To run all integration tests, use `make integration`.
* To run integration tests for the specified target only, use `make <target>`,
    for instance `make api_authentication`.

## The general structure of integration tests

Integration tests are grouped in _targets_. You will find them under
`test/integration/targets` directory. Every test target is a role that will be
executed when running `ansible-test integration`. A target should contain
a `main.yml` file with your tests, i.e. this is where you put the tasks 
that invoke the modules from the collection, and make assertions about the results.
 
`main.yml` will typically contain many tasks using `servicenow.itsm`
modules. Since all the modules require `instance` parameter, and
all instance parameters can be specified via environment variables,
we can improve readability and reduce lines of code by declaring
a shared `environment` for all tasks, like this:
    
```yaml
---
# Environment for the block of tasks that follow.
# Values of env vars are specified via the corresponding vars from integration_config.yml.
- environment:
    SN_HOST: "{{ sn_host }}"
    SN_USERNAME: "{{ sn_username }}"
    SN_PASSWORD: "{{ sn_password }}"
    SN_CLIENT_ID: "{{ sn_client_id }}"
    SN_CLIENT_SECRET: "{{ sn_client_secret }}"

  block:
    - servicenow.itsm.problem_info:
        # No need to define instance parameter here, or for
        # any of the subsequent tasks.
      
        # Other params relevant to your test
        <other_module_params_here>
      register: result
    # Assertions about the result of the previous task
    - ansible.builtin.assert:
        that:
           - result.records == []
```

Then, we can define our tests (i.e. tasks that invoke relevant modules from the
collection, followed by assertions about their results) under `block`, 
as illustrated in the example above.

### Avoiding reliance on any pre-existing PDI data

By default, Personal Developer Instances are populated with some initial data.
To avoid failures due to PDIs being populated with different test data, integration
tests should, wherever possible, avoid relying on any pre-existing data. 
For instance, the assertions should avoid:

* checking the total record count, or
* checking the presence of a record that was not created by the module under test.  

### Testing for expected module failures

When we expect a module to fail, we specify `ignore_errors: true` for the task.
Then, to verify that the failure is due to the expected reason, we need to check
the error message, for example:

```yaml
- name: Fail when multiple records match
  servicenow.itsm.incident:
    number: INC01293
    state: new
  register: result
  ignore_errors: true
- assert:
    that:
      - result is failed
      - "'2 incidents match the query' in result.msg"
```
 
Note that you should not check for failures due to static module argspec 
validation in the integration tests. These must be covered with unit tests.


## Integration tests for module pairs

Integration tests for every (module, info_module) pair should be under the
same target. For example, integration tests for modules `incident` and 
`incident_info` go under the `incident` target. If we're not testing a
module pair, a descriptive target name should be used.

To test a module, we typically need its info counterpart module as well.
The info module helps us to verify, for instance, that the module did not
make any changes when running in check mode.

Integration tests for every module pair should verify that:

* resource retrieval,
* resource creation,
* resource modification, and
* resource removal

work as expected.

### Resource retrieval

Resource retrieval tests are the tests for info modules, i.e. modules that do
not modify the state of the ServiceNow resources.

They should check that:

* When no parameters are given to the info module, the resulting record list
    contains all the records.
* When parameters that pin down what needs to be returned are given to the module,
    only the expected records are returned.
* An empty list of records is returned when the specified resource does not exist.

### Resource creation, modification and removal
 
For every action that changes the state of a ServiceNow resource, there
should be tasks to verify that:

* In __check mode__, a resource has not been created/modified/removed,
    but the result gives us a hint of what would be changed.
* In __normal mode__, a resource has been created/modified/removed,
    and the result is what we expect.
* With __idempotency check__, we verify that another invocation of the module
    in the normal mode (with the same parameters), results in `changed=False` 
    and the same result as in the first invocation.
 
Info modules can be used to aid in these tests, so that we can verify
that a module has (or has not) modified a resource as expected.
