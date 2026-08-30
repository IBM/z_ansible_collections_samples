.. Created with antsibull-docs 2.14.0

servicenow.itsm.api module -- Manage ServiceNow POST, PATCH and DELETE requests
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.api``.

New in servicenow.itsm 2.0.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Create, delete or update a ServiceNow record from the given resource.
- For more information, refer to the ServiceNow REST Table API documentation at \ `https://docs.servicenow.com/bundle/tokyo-application-development/page/integrate/inbound-rest/concept/c\_RESTAPI.html <https://docs.servicenow.com/bundle/tokyo-application-development/page/integrate/inbound-rest/concept/c_RESTAPI.html>`__.

This module has a corresponding action plugin.







Parameters
----------

.. raw:: html

  <table style="width: 100%;">
  <thead>
    <tr>
    <th colspan="2"><p>Parameter</p></th>
    <th><p>Comments</p></th>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-action"></div>
      <p style="display: inline;"><strong>action</strong></p>
      <a class="ansibleOptionLink" href="#parameter-action" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>The action to perform.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;post&#34;</code></p></li>
        <li><p><code>&#34;patch&#34;</code></p></li>
        <li><p><code>&#34;delete&#34;</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-api_path"></div>
      <p style="display: inline;"><strong>api_path</strong></p>
      <a class="ansibleOptionLink" href="#parameter-api_path" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.5.0</i></p>
    </td>
    <td valign="top">
      <p>The path of the service which a record is to be created, updated or deleted from.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>resource</code>.</p>
      <p>Require one of <code class='docutils literal notranslate'>resource</code> or <code class='docutils literal notranslate'>api_path</code>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-data"></div>
      <p style="display: inline;"><strong>data</strong></p>
      <a class="ansibleOptionLink" href="#parameter-data" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The data that we want to update or create the resource with.</p>
      <p>Mutually exclusive with <em>template</em>.</p>
      <p>Only relevant if <em>action==patch</em> or <em>action==post</em>.</p>
      <p>A Dict consists of resource&#x27;s column names as keys (such as description, number, priority, and so on) and the patching values as values (the value we want to change the column to).</p>
      <p>When updating a resource&#x27;s record, if no datum is specified for a specific column, the value of that column will remain intact.</p>
      <p>When creating a resource&#x27;s record, if no datum is specified for a specific column, the default value of the column will be used.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">{}</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance"></div>
      <p style="display: inline;"><strong>instance</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>ServiceNow instance information.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/access_token"></div>
      <p style="display: inline;"><strong>access_token</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/access_token" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.3.0</i></p>
    </td>
    <td valign="top">
      <p>Access token obtained via OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_ACCESS_TOKEN</code> environment variable will be used.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/api_path"></div>
      <p style="display: inline;"><strong>api_path</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/api_path" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.4.0</i></p>
    </td>
    <td valign="top">
      <p>Change the API endpoint of SNOW instance from default &#x27;api/now&#x27;.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;api/now&#34;</code></p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/client_id"></div>
      <p style="display: inline;"><strong>client_id</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/client_id" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>ID of the client application used for OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_CLIENT_ID</code> environment variable will be used.</p>
      <p>If provided, it requires <em>client_secret</em>.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/client_secret"></div>
      <p style="display: inline;"><strong>client_secret</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/client_secret" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Secret associated with <em>client_id</em>. Used for OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_CLIENT_SECRET</code> environment variable will be used.</p>
      <p>If provided, it requires <em>client_id</em>.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/custom_headers"></div>
      <p style="display: inline;"><strong>custom_headers</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/custom_headers" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.4.0</i></p>
    </td>
    <td valign="top">
      <p>A dictionary containing any extra headers which will be passed with the request.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/grant_type"></div>
      <p style="display: inline;"><strong>grant_type</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/grant_type" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.1.0</i></p>
    </td>
    <td valign="top">
      <p>Grant type used for OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_GRANT_TYPE</code> environment variable will be used.</p>
      <p>Since version 2.3.0, it no longer has a default value in the argument specifications.</p>
      <p>If not set by any means, the default value (that is, <em>password</em>) will be set internally to preserve backwards compatibility.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;password&#34;</code></p></li>
        <li><p><code>&#34;refresh_token&#34;</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/host"></div>
      <p style="display: inline;"><strong>host</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/host" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>The ServiceNow host name.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_HOST</code> environment variable will be used.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/password"></div>
      <p style="display: inline;"><strong>password</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/password" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Password used for authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_PASSWORD</code> environment variable will be used.</p>
      <p>Required when using basic authentication or when <em>grant_type=password</em>.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/refresh_token"></div>
      <p style="display: inline;"><strong>refresh_token</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/refresh_token" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.1.0</i></p>
    </td>
    <td valign="top">
      <p>Refresh token used for OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_REFRESH_TOKEN</code> environment variable will be used.</p>
      <p>Required when <em>grant_type=refresh_token</em>.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/timeout"></div>
      <p style="display: inline;"><strong>timeout</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/timeout" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">float</span>
      </p>
    </td>
    <td valign="top">
      <p>Timeout in seconds for the connection with the ServiceNow instance.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_TIMEOUT</code> environment variable will be used.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/username"></div>
      <p style="display: inline;"><strong>username</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/username" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Username used for authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_USERNAME</code> environment variable will be used.</p>
      <p>Required when using basic authentication or when <em>grant_type=password</em>.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-instance/validate_certs"></div>
      <p style="display: inline;"><strong>validate_certs</strong></p>
      <a class="ansibleOptionLink" href="#parameter-instance/validate_certs" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.3.0</i></p>
    </td>
    <td valign="top">
      <p>If host&#x27;s certificate is validated or not.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>false</code></p></li>
        <li><p><code style="color: blue;"><b>true</b></code> <span style="color: blue;">← (default)</span></p></li>
      </ul>

    </td>
  </tr>

  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-query_params"></div>
      <p style="display: inline;"><strong>query_params</strong></p>
      <a class="ansibleOptionLink" href="#parameter-query_params" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.1.0</i></p>
    </td>
    <td valign="top">
      <p>Query parameters that may be used on POST or PATCH request.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">{}</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-resource"></div>
      <p style="display: inline;"><strong>resource</strong></p>
      <a class="ansibleOptionLink" href="#parameter-resource" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The name of the table in which a record is to be created, updated or deleted from.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>api_path</code>.</p>
      <p>Require one of <code class='docutils literal notranslate'>resource</code> or <code class='docutils literal notranslate'>api_path</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-sys_id"></div>
      <p style="display: inline;"><strong>sys_id</strong></p>
      <a class="ansibleOptionLink" href="#parameter-sys_id" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Required if <em>action==patch</em> or <em>action==delete</em>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-template"></div>
      <p style="display: inline;"><strong>template</strong></p>
      <a class="ansibleOptionLink" href="#parameter-template" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Provide a valid YAML template definition file for creating or updating a record.</p>
      <p>Provides built-in template processing capabilities as an alternative to its data parameter.</p>
      <p>Mutually exclusive with <em>data</em>.</p>
      <p>If template starts with <code class='docutils literal notranslate'>"/"</code>, it is assumed you have specified absolute path to the file. Otherwise, it is assumed you have specified relative path to the file.</p>
      <p>Template file needs to be present on the Ansible Controller&#x27;s system. Otherwise, an error is raised.</p>
    </td>
  </tr>
  </tbody>
  </table>





See Also
--------

* `servicenow.itsm.api\_info <api_info_module.rst>`__

  Manage ServiceNow GET requests.

Examples
--------

.. code-block:: yaml

    - name: Create a record in table incident with specified short_description (which is read from data)
      servicenow.itsm.api:
        resource: incident
        action: post
        data:
          short_description: my-incident
      register: result

    - name: Create a record in table incident with column values set in template, located in Ansible controller file system
      servicenow.itsm.api:
        resource: incident
        action: post
        template: '/testing/deployment.j2'
      register: result

    - name: Update a record with given sys_id in table incident with template, located in Ansible controller file system
      servicenow.itsm.api:
        resource: incident
        action: patch
        sys_id: 46b66a40a9fe198101f243dfbc79033d
        template: '/testing/deployment.j2'
      register: result

    - name: Update column short_description (specified in data) in table incident of a record with given sys_id
      servicenow.itsm.api:
        resource: incident
        action: patch
        sys_id: 46b66a40a9fe198101f243dfbc79033d
        data:
          short_description: my-incident-updated
      register: result

    - name: Delete the resource the table incident with given sys_id
      servicenow.itsm.api:
        resource: incident
        action: delete
        sys_id: 46b66a40a9fe198101f243dfbc79033d
      register: result

    - name: Create a record in the table sc_req_item and set short_description's value to demo-description2
      servicenow.itsm.api:
        resource: sc_req_item
        action: post
        data:
          short_description: demo-description2
      register: result

    - name: Create a record in the table sc_req_item and set short_description's value to demo-description2
      servicenow.itsm.api:
        resource: sc_req_item
        action: post
        data:
          short_description: demo-description2
      register: result

    - name: create user (object with encrypted fields)
      servicenow.itsm.api:
        resource: sys_user
        action: post
        query_params:
          sysparm_input_display_value: true
        data:
          user_name: "demo_username"
          user_password: "demo_password"
          first_name: "first_name"
          last_name: Demouser
          department: IT
          email: "demo_username@example.com"
          title: Demo user
      register: user

    - name: Create a record in sc_req_item with column values set in template, located in Ansible controller file system
      servicenow.itsm.api:
        resource: sc_req_item
        action: post
        template: '/testing/deployment.j2'
      register: result

    - name: Delete a record by sys_id from table sc_req_item
      servicenow.itsm.api:
        resource: sc_req_item
        action: delete
        sys_id: b82adae197201110949235dfe153afec
      register: result

    - name: Create a record in cmdb service using api_path
      servicenow.itsm.api:
        api_path: api/now/cmdb/instance/cmdb_ci_linux_server
        action: post
        data:
          attributes:
            name: "linux99"
            firewall_status: "intranet"
        source: "ServiceNow"




Return Values
-------------
The following are the fields unique to this module:

.. raw:: html

  <table style="width: 100%;">
  <thead>
    <tr>
    <th><p>Key</p></th>
    <th><p>Description</p></th>
  </tr>
  </thead>
  <tbody>
  <tr>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="return-record"></div>
      <p style="display: inline;"><strong>record</strong></p>
      <a class="ansibleOptionLink" href="#return-record" title="Permalink to this return value"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The created, updated or deleted record.</p>
      <p style="margin-top: 8px;"><b>Returned:</b> success</p>
      <p style="margin-top: 8px; color: blue; word-wrap: break-word; word-break: break-all;"><b style="color: black;">Sample:</b> <code>{&#34;active&#34;: &#34;true&#34;, &#34;activity_due&#34;: &#34;&#34;, &#34;additional_assignee_list&#34;: &#34;&#34;, &#34;approval&#34;: &#34;not requested&#34;, &#34;approval_history&#34;: &#34;&#34;, &#34;approval_set&#34;: &#34;&#34;, &#34;assigned_to&#34;: &#34;&#34;, &#34;assignment_group&#34;: &#34;&#34;, &#34;business_duration&#34;: &#34;&#34;, &#34;business_impact&#34;: &#34;&#34;, &#34;business_service&#34;: &#34;&#34;, &#34;business_stc&#34;: &#34;&#34;, &#34;calendar_duration&#34;: &#34;&#34;, &#34;calendar_stc&#34;: &#34;&#34;, &#34;caller_id&#34;: &#34;&#34;, &#34;category&#34;: &#34;inquiry&#34;, &#34;cause&#34;: &#34;&#34;, &#34;caused_by&#34;: &#34;&#34;, &#34;child_incidents&#34;: &#34;0&#34;, &#34;close_code&#34;: &#34;&#34;, &#34;close_notes&#34;: &#34;&#34;, &#34;closed_at&#34;: &#34;&#34;, &#34;closed_by&#34;: &#34;&#34;, &#34;cmdb_ci&#34;: &#34;&#34;, &#34;comments&#34;: &#34;&#34;, &#34;comments_and_work_notes&#34;: &#34;&#34;, &#34;company&#34;: &#34;&#34;, &#34;contact_type&#34;: &#34;&#34;, &#34;contract&#34;: &#34;&#34;, &#34;correlation_display&#34;: &#34;&#34;, &#34;correlation_id&#34;: &#34;&#34;, &#34;delivery_plan&#34;: &#34;&#34;, &#34;delivery_task&#34;: &#34;&#34;, &#34;description&#34;: &#34;&#34;, &#34;due_date&#34;: &#34;&#34;, &#34;escalation&#34;: &#34;0&#34;, &#34;expected_start&#34;: &#34;&#34;, &#34;follow_up&#34;: &#34;&#34;, &#34;group_list&#34;: &#34;&#34;, &#34;hold_reason&#34;: &#34;&#34;, &#34;impact&#34;: &#34;3&#34;, &#34;incident_state&#34;: &#34;1&#34;, &#34;knowledge&#34;: &#34;false&#34;, &#34;location&#34;: &#34;&#34;, &#34;made_sla&#34;: &#34;true&#34;, &#34;notify&#34;: &#34;1&#34;, &#34;number&#34;: &#34;INC0010204&#34;, &#34;opened_at&#34;: &#34;2022-07-06 08:53:05&#34;, &#34;opened_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;order&#34;: &#34;&#34;, &#34;origin_id&#34;: &#34;&#34;, &#34;origin_table&#34;: &#34;&#34;, &#34;parent&#34;: &#34;&#34;, &#34;parent_incident&#34;: &#34;&#34;, &#34;priority&#34;: &#34;5&#34;, &#34;problem_id&#34;: &#34;&#34;, &#34;reassignment_count&#34;: &#34;0&#34;, &#34;reopen_count&#34;: &#34;0&#34;, &#34;reopened_by&#34;: &#34;&#34;, &#34;reopened_time&#34;: &#34;&#34;, &#34;resolved_at&#34;: &#34;&#34;, &#34;resolved_by&#34;: &#34;&#34;, &#34;rfc&#34;: &#34;&#34;, &#34;route_reason&#34;: &#34;&#34;, &#34;service_offering&#34;: &#34;&#34;, &#34;severity&#34;: &#34;3&#34;, &#34;short_description&#34;: &#34;my-incident&#34;, &#34;sla_due&#34;: &#34;&#34;, &#34;state&#34;: &#34;1&#34;, &#34;subcategory&#34;: &#34;&#34;, &#34;sys_class_name&#34;: &#34;incident&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2022-07-06 08:53:05&#34;, &#34;sys_domain&#34;: &#34;global&#34;, &#34;sys_domain_path&#34;: &#34;/&#34;, &#34;sys_id&#34;: &#34;35b5fb4197245110949235dfe153af06&#34;, &#34;sys_mod_count&#34;: &#34;0&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2022-07-06 08:53:05&#34;, &#34;task_effective_number&#34;: &#34;INC0010204&#34;, &#34;time_worked&#34;: &#34;&#34;, &#34;universal_request&#34;: &#34;&#34;, &#34;upon_approval&#34;: &#34;proceed&#34;, &#34;upon_reject&#34;: &#34;cancel&#34;, &#34;urgency&#34;: &#34;3&#34;, &#34;user_input&#34;: &#34;&#34;, &#34;watch_list&#34;: &#34;&#34;, &#34;work_end&#34;: &#34;&#34;, &#34;work_notes&#34;: &#34;&#34;, &#34;work_notes_list&#34;: &#34;&#34;, &#34;work_start&#34;: &#34;&#34;}</code></p>
    </td>
  </tr>
  </tbody>
  </table>




Authors
~~~~~~~

- Tjaž Eržen (@tjazsch)
- Jure Medvešek (@juremedvesek)



Collection links
~~~~~~~~~~~~~~~~

* `Issue Tracker <https://github.com/ansible-collections/servicenow.itsm/issues>`__
* `Repository (Sources) <https://github.com/ansible-collections/servicenow.itsm>`__
