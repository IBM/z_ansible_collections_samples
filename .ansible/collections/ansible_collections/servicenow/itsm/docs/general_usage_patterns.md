# General usage patterns

While the ServiceNow ITSM Ansible Collection modules allow us to perform
different tasks, they all share the same high-level interface. In this
document, we will look at the general usage patterns and talk about scenarios
in which we should use them.


## General structure

Each regular module has four different sets of parameters that contain
information about the ServiceNow instance we will manage, record identifiers,
commonly-used resource fields, and a catch-all parameter for custom data.

The first set contains a single parameter called `instance` that includes:

 1. Instance address, for example `https://dev12345.service-now.com/`. This value must start with `https://`.
 2. Username and password that Ansible modules should use to authenticate with
    the ServiceNow instance.
 3. The optional client id and secret that indicate modules should use OAuth
    when authenticating.

Parameters in the second set allow us to select an existing record to manage.
All modules support identifying records by system id (the `sys_id` parameter).
Because those are often awkward to work with, most modules know how to select
records by their number. Those numbers are readily available from the
ServiceNow user interface.

The third set of parameters offers convenient access to commonly-used record
fields. Some of the parameters from the third set contain additional validation
logic that helps guide playbook authors. For example, the `state` parameter
only accepts valid state names.

The `other` parameter forms the last set of parameters. This free-form
parameter allows us to pass any data to the backend. This parameter comes in
handy when we want to set data in user-created custom columns.

Info modules are more spartan and only have two sets of parameters: an
`instance` parameter and record selectors like `sys_id` and `number`.

In practice, tasks using a module from ServiceNow ITSM Ansible Collection looks
like this:

    ---
    - name: Sample playbook
      hosts: localhost
      gather_facts: false

      tasks:
        - name: Create a new incident
          servicenow.itsm.incident:
            # Instance data
            instance:
              host: https://dev12345.service-now.com
              username: user
              password: pass
              client_id: cid
              client_secret: csecret

            # No sys_id or number because we are creating a new incident

            # Common parameters
            caller: admin
            short_description: Demo incident
            impact: low
            urgency: low

            # Other data
            other:
              problem_id: "{{ problem.record.sys_id }}"

        - name: Update an existing incident
          servicenow.itsm.incident:
            # Instance data
            instance:
              host: https://dev12345.service-now.com
              username: user
              password: pass
              client_id: cid
              client_secret: csecret

            # Record identificator
            number: INC1234567

            # Common parameters
            state: in_progress
            impact: high
            urgency: high

        - name: Delete incident ticket (rare operation)
          servicenow.itsm.incident:
            # Instance data
            instance:
              host: https://dev12345.service-now.com
              username: user
              password: pass
              client_id: cid
              client_secret: csecret

            # Record identificator
            number: INC1234567

            # Common parameters
            state: absent


## Removing instance data from playbooks

As can be seen from the previous example, we need to pass the instance data to
each task. And we can probably all agree that this is not an ideal situation to
be in because:

 1. If the authentication data changes, we need to update multiple tasks.
 2. We store passwords and client secrets in plain text.

To make things a bit easier to update and a lot more secure, modules from the
ServiceNow ITSM Ansible Collection can retrieve this from environment
variables. Instead of adding `password` to each task, we can set the
`SN_PASSWORD` environment variable. The same thing holds for all `instance`
options: we can either provide a `param` value or export the `SN_PARAM`
environment variable.

For example, this is how we would store all ServiceNow instance information in
environment variables:

    $ export SN_HOST='https://dev12345.service-now.com'
    $ export SN_USERNAME='user'
    $ export SN_PASSWORD='pass'
    $ export SN_CLIENT_ID='cid'
    $ export SN_CLIENT_SECRET='csecret'

Make sure that `SN_HOST` environment variable starts with `https://`.
Once we set all instance parameters through the environment variables, we can
omit the `instance` parameter entirely and end up with a much shorter playbook:

    ---
    - name: Sample playbook
      hosts: localhost
      gather_facts: false

      tasks:
        - name: Create a new incident
          servicenow.itsm.incident:
            # No sys_id or number because we are creating a new incident

            # Common parameters
            caller: admin
            short_description: Demo incident
            impact: low
            urgency: low

            # Other data
            other:
              problem_id: "{{ problem.record.sys_id }}"

        - name: Update an existing incident
          servicenow.itsm.incident:
            # Record identificator
            number: INC1234567

            # Common parameters
            state: in_progress
            impact: high
            urgency: high

        - name: Delete incident ticket (rare operation)
          servicenow.itsm.incident:
            # Record identificator
            number: INC1234567

            # Common parameters
            state: absent


## Using modules from Automation Controller

If we want to manage the ServiceNow instance from Automation controller, we need to
create a custom credential type that will hold our instance data. The minimal
credential type has the following input configuration:

    fields:
      - id: SN_HOST
        type: string
        label: Snow Instance
      - id: SN_USERNAME
        type: string
        label: Username
      - id: SN_PASSWORD
        type: string
        label: Password
        secret: true
    required:
      - SN_HOST
      - SN_USERNAME
      - SN_PASSWORD

The configuration above informs Automation controller what pieces of data we would like
to store in our credentials. And to get the credentials to the playbook, we
also need to have the following injector configuration:

    env:
      SN_HOST: '{{ SN_HOST }}'
      SN_PASSWORD: '{{ SN_PASSWORD }}'
      SN_USERNAME: '{{ SN_USERNAME }}'

When we assign a secret to a template, Automation controller will set the environment
variables from the injector configuration before running it.

If we want to use OAuth for authentication, we need to add client id and secret
fields to the input and injector configuration. But we will leave this part as
an exercise for the reader.

Do note that we can use the same credential type for running playbooks and
fetching inventory.
