# Contributing

## Getting Started

General information about setting up your Python environment, testing modules,
Ansible coding styles, and more can be found in the [Ansible Community Guide](
https://docs.ansible.com/ansible/latest/community/index.html).


## ServiceNow ITSM Collections

### servicenow.itsm

The Ansible Collection for ServiceNow IT Service Management ([ITSM](https://www.servicenow.com/products/itsm.html)) includes a variety of Ansible content to help automate the management of ServiceNow IT Service Management.

New modules and plugins developed by the community should be proposed to `servicenow.itsm`.

## Submitting Issues
All software has bugs, and the `servicenow.itsm` collection is no exception. When you find a bug,
you can help tremendously by [telling us about it](https://github.com/ansible-collections/servicenow.itsm/issues).

If you should discover that the bug you're trying to file already exists in an issue,
you can help by verifying the behavior of the reported bug with a comment in that
issue, or by reporting any additional information.

## Environment setup
1. Sign up for a [ServiceNow ID](https://signon.service-now.com/x_snc_sso_auth.do#/sign-up) 
1. Request and setup an Instance. 
1. When the instance is ready, you will be redirected to the Developer Portal, for example `https://dev12345.service-now.com/`. In All, search for `Application Registry` then New - Create an OAuth API endpoint for external clients.
1. Ensure that you have changed the Integration tests configuration file accordingly with your Personal Developer Instance credentials for the connection and access. A template is available [here](https://github.com/ansible-collections/servicenow.itsm/blob/main/tests/integration/integration_config.yml.template).
1. For more information, check [General usage patterns](https://github.com/ansible-collections/servicenow.itsm/blob/main/docs/general_usage_patterns.md).
1. If you are running unit tests or integration tests included in `servicenow.itsm` source using `make` command, it is advised to installed container runtimes such as Podman or Docker. For more information please refer [Ansible Testing Guidelines](https://docs.ansible.com/ansible/latest/dev_guide/testing_integration.html#tests-in-containers).


## Writing New Code

New modules and plugins should be submitted to the [servicenow.itsm](https://github.com/ansible-collections/servicenow.itsm) collection.

### Static analysis
For new features and bug fixes on existing modules and plugins, clone this repository and try to run unit tests and integration tests by following [these instructions](https://docs.ansible.com/ansible/latest/community/create_pr_quick_start.html). When you get to this part, we use a wide variety of linters and static code analysis tools to ensure our code base stays healthy. To run all checks, we can execute the following command:

```
(venv) $ make sanity
```
This command will take care of installing requirements and running them.

### Unit tests
We use unit tests to test the logic and implementation of our helper functions and various Ansible plugins that are part of the `servicenow.itsm` collection.
```
(venv) $ make units
```

### Integration tests
```
(venv) $ make integration
```
With the help of integration tests, we make sure that the plugins from the ServiceNow Ansible Collection work as expected when we run them against a ServiceNow instance. We use ansible-test integration, which executes our test targets (roles) that contain test playbooks.
More information regarding integration tests can be found [here](https://github.com/ansible-collections/servicenow.itsm/blob/main/docs/integration_testing.md).

Expected test criteria:
* Resource creation under check mode
* Resource creation
* Resource creation again (idempotency) under check mode
* Resource creation again (idempotency)
* Resource modification under check mode
* Resource modification
* Resource modification again (idempotency) under check mode
* Resource modification again (idempotency)
* Resource deletion under check mode
* Resource deletion
* Resource deletion (of a non-existent resource) under check mode
* Resource deletion (of a non-existent resource)

Where modules and plugins have multiple parameters we recommend running through the 4-step modification cycle for each parameter the module accepts, as well as a modification cycle where as most, if not all, parameters are modified at the same time.

For general information on running the integration tests see the
[Integration Tests page of the Module Development Guide](https://docs.ansible.com/ansible/devel/dev_guide/testing_integration.html#testing-integration). For questions about writing tests the Ansible ServiceNow ITSM can be found on Libera.Chat IRC as detailed below.


### Code of Conduct
The `servicenow.itsm` collection follows the Ansible project's
[Code of Conduct](https://docs.ansible.com/ansible/devel/community/code_of_conduct.html).
Please read and familiarize yourself with this document.

### IRC
Our IRC channels may require you to register your nickname. If you receive an error when you connect, see
[Libera.Chat's Nickname Registration guide](https://libera.chat/guides/registration) for instructions.

The `#ansible-community` channel on [libera.chat](https://libera.chat/) IRC is the main and official place to discuss the use and development of the `serviceNow.itsm` collection.

### For more information
* [Ansible Collection overview](https://github.com/ansible-collections/overview)
* [Ansible User guide](https://docs.ansible.com/ansible/latest/user_guide/index.html)
* [Ansible Developer guide](https://docs.ansible.com/ansible/latest/dev_guide/index.html)
* [Ansible Community code of conduct](https://docs.ansible.com/ansible/latest/community/code_of_conduct.html)
