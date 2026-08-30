.. Created with antsibull-docs 2.14.0

servicenow.itsm.problem_task_info module -- List ServiceNow problem tasks
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.problem_task_info``.

New in servicenow.itsm 1.3.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Retrieve information about ServiceNow problem tasks.
- For more information, refer to the ServiceNow problem management documentation at \ `https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/concept/c\_ProblemManagement.html <https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/concept/c_ProblemManagement.html>`__.








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
      <div class="ansibleOptionAnchor" id="parameter-number"></div>
      <p style="display: inline;"><strong>number</strong></p>
      <a class="ansibleOptionLink" href="#parameter-number" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Number of the record to retrieve.</p>
      <p>Note that contrary to <em>sys_id</em>, <em>number</em> may not uniquely identify a record.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-query"></div>
      <p style="display: inline;"><strong>query</strong></p>
      <a class="ansibleOptionLink" href="#parameter-query" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>Provides a set of operators for use with filters, condition builders, and encoded queries.</p>
      <p>The data type of a field determines what operators are available for it. Refer to the ServiceNow Available Filters Queries documentation at <a href='https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html'>https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html</a>.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>sysparm_query</code>.</p>
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
      <p>Unique identifier of the record to retrieve.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-sysparm_display_value"></div>
      <p style="display: inline;"><strong>sysparm_display_value</strong></p>
      <a class="ansibleOptionLink" href="#parameter-sysparm_display_value" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.0.0</i></p>
    </td>
    <td valign="top">
      <p>Return field display values <code class='docutils literal notranslate'>true</code>, actual values <code class='docutils literal notranslate'>false</code>, or both <code class='docutils literal notranslate'>all</code>.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;true&#34;</code></p></li>
        <li><p><code style="color: blue;"><b>&#34;false&#34;</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>&#34;all&#34;</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-sysparm_query"></div>
      <p style="display: inline;"><strong>sysparm_query</strong></p>
      <a class="ansibleOptionLink" href="#parameter-sysparm_query" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.0.0</i></p>
    </td>
    <td valign="top">
      <p>An encoded query string used to filter the results as an alternative to <code class='docutils literal notranslate'>query</code>.</p>
      <p>Refer to the ServiceNow Available Filters Queries documentation at <a href='https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html'>https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html</a>.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_SYSPARM_QUERY</code> environment, if specified.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>query</code>.</p>
    </td>
  </tr>
  </tbody>
  </table>





See Also
--------

* `servicenow.itsm.problem\_task <problem_task_module.rst>`__

  Manage ServiceNow problem tasks.
* `servicenow.itsm.problem <problem_module.rst>`__

  Manage ServiceNow problems.
* `servicenow.itsm.problem\_info <problem_info_module.rst>`__

  List ServiceNow problems.

Examples
--------

.. code-block:: yaml

    - name: Retrieve all problem tasks
      servicenow.itsm.problem_task_info:
      register: result

    - name: Retrieve a specific problem task by its sys_id
      servicenow.itsm.problem_task_info:
        sys_id: 471bfbc7a9fe198101e77a3e10e5d47f
      register: result

    - name: Retrieve problem tasks by number
      servicenow.itsm.problem_task_info:
        number: PTASK0007601
      register: result

    - name: Retrieve problem tasks that do not contain SAP in its short description by using field query
      servicenow.itsm.problem_task_info:
        query:
          - short_description: NOT LIKE SAP
      register: result

    - name: Retrieve problem tasks that do not contain SAP in its short description by using field sysparm_query
      servicenow.itsm.problem_task_info:
        sysparm_query: short_descriptionNOT LIKESAP
      register: result

    - name: Retrieve new problem tasks assigned to abel.tuter or bertie.luby
      servicenow.itsm.problem_task_info:
        query:
          - state: = new
            assigned_to: = abel.tuter
          - state: = new
            assigned_to: = bertie.luby




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
      <div class="ansibleOptionAnchor" id="return-records"></div>
      <p style="display: inline;"><strong>records</strong></p>
      <a class="ansibleOptionLink" href="#return-records" title="Permalink to this return value"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=string</span>
      </p>
    </td>
    <td valign="top">
      <p>A list of problem records.</p>
      <p style="margin-top: 8px;"><b>Returned:</b> success</p>
      <p style="margin-top: 8px; color: blue; word-wrap: break-word; word-break: break-all;"><b style="color: black;">Sample:</b> <code>[{&#34;active&#34;: &#34;true&#34;, &#34;activity_due&#34;: &#34;&#34;, &#34;additional_assignee_list&#34;: &#34;&#34;, &#34;approval&#34;: &#34;not requested&#34;, &#34;approval_history&#34;: &#34;&#34;, &#34;approval_set&#34;: &#34;&#34;, &#34;assigned_to&#34;: &#34;7e3bbb173b331300ad3cc9bb34efc4a8&#34;, &#34;assignment_group&#34;: &#34;&#34;, &#34;business_duration&#34;: &#34;&#34;, &#34;business_service&#34;: &#34;&#34;, &#34;calendar_duration&#34;: &#34;&#34;, &#34;cause_code&#34;: &#34;&#34;, &#34;cause_notes&#34;: &#34;&#34;, &#34;close_code&#34;: &#34;&#34;, &#34;close_notes&#34;: &#34;&#34;, &#34;closed_at&#34;: &#34;&#34;, &#34;closed_by&#34;: &#34;&#34;, &#34;cmdb_ci&#34;: &#34;26da329f0a0a0bb400f69d8159bc753d&#34;, &#34;comments&#34;: &#34;&#34;, &#34;comments_and_work_notes&#34;: &#34;&#34;, &#34;company&#34;: &#34;&#34;, &#34;contact_type&#34;: &#34;&#34;, &#34;contract&#34;: &#34;&#34;, &#34;correlation_display&#34;: &#34;&#34;, &#34;correlation_id&#34;: &#34;&#34;, &#34;delivery_plan&#34;: &#34;&#34;, &#34;delivery_task&#34;: &#34;&#34;, &#34;description&#34;: &#34;&#34;, &#34;due_date&#34;: &#34;&#34;, &#34;escalation&#34;: &#34;0&#34;, &#34;expected_start&#34;: &#34;&#34;, &#34;fix_notes&#34;: &#34;&#34;, &#34;follow_up&#34;: &#34;&#34;, &#34;group_list&#34;: &#34;&#34;, &#34;impact&#34;: &#34;low&#34;, &#34;knowledge&#34;: &#34;false&#34;, &#34;location&#34;: &#34;&#34;, &#34;made_sla&#34;: &#34;true&#34;, &#34;number&#34;: &#34;PTASK0010005&#34;, &#34;opened_at&#34;: &#34;2020-12-17 10:21:49&#34;, &#34;opened_by&#34;: &#34;d3dbbf173b331300ad3cc9bb34efc466&#34;, &#34;order&#34;: &#34;&#34;, &#34;other_reason&#34;: &#34;&#34;, &#34;parent&#34;: &#34;&#34;, &#34;priority&#34;: &#34;2&#34;, &#34;problem&#34;: &#34;d7296d02c0a801670085e737da016e70&#34;, &#34;problem_task_type&#34;: &#34;rca&#34;, &#34;reassignment_count&#34;: &#34;0&#34;, &#34;reopen_count&#34;: &#34;1&#34;, &#34;reopened_at&#34;: &#34;2020-12-17 10:23:10&#34;, &#34;reopened_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;route_reason&#34;: &#34;&#34;, &#34;service_offering&#34;: &#34;&#34;, &#34;short_description&#34;: &#34;SAP outage, please investigate the cause&#34;, &#34;sla_due&#34;: &#34;&#34;, &#34;started_at&#34;: &#34;2020-12-17 10:23:14&#34;, &#34;started_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;state&#34;: &#34;154&#34;, &#34;sys_class_name&#34;: &#34;problem_task&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2020-12-17 10:22:25&#34;, &#34;sys_domain&#34;: &#34;global&#34;, &#34;sys_domain_path&#34;: &#34;/&#34;, &#34;sys_id&#34;: &#34;5f6bec57531063004247ddeeff7b1216&#34;, &#34;sys_mod_count&#34;: &#34;5&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2020-12-17 10:27:14&#34;, &#34;task_effective_number&#34;: &#34;PTASK0010005&#34;, &#34;time_worked&#34;: &#34;&#34;, &#34;universal_request&#34;: &#34;&#34;, &#34;upon_approval&#34;: &#34;proceed&#34;, &#34;upon_reject&#34;: &#34;cancel&#34;, &#34;urgency&#34;: &#34;low&#34;, &#34;user_input&#34;: &#34;&#34;, &#34;vendor&#34;: &#34;&#34;, &#34;watch_list&#34;: &#34;&#34;, &#34;work_end&#34;: &#34;&#34;, &#34;work_notes&#34;: &#34;&#34;, &#34;work_notes_list&#34;: &#34;&#34;, &#34;work_start&#34;: &#34;&#34;, &#34;workaround&#34;: &#34;&#34;}]</code></p>
    </td>
  </tr>
  </tbody>
  </table>




Authors
~~~~~~~

- Manca Bizjak (@mancabizjak)
- Miha Dolinar (@mdolin)
- Tadej Borovsak (@tadeboro)



Collection links
~~~~~~~~~~~~~~~~

* `Issue Tracker <https://github.com/ansible-collections/servicenow.itsm/issues>`__
* `Repository (Sources) <https://github.com/ansible-collections/servicenow.itsm>`__
