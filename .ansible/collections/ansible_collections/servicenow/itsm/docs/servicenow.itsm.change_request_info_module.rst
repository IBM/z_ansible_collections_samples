.. Created with antsibull-docs 2.14.0

servicenow.itsm.change_request_info module -- List ServiceNow change requests
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.change_request_info``.

New in servicenow.itsm 1.0.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Retrieve information about ServiceNow change requests.
- For more information, refer to the ServiceNow change management documentation at \ `https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c\_ITILChangeManagement.html <https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_ITILChangeManagement.html>`__.








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
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping"></div>
      <p style="display: inline;"><strong>change_request_mapping</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.3.0</i></p>
    </td>
    <td valign="top">
      <p>User mapping for <em>Change request</em> object, where user can override Choice Lists values for objects.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping/category"></div>
      <p style="display: inline;"><strong>category</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping/category" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The category of the change request.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping/impact"></div>
      <p style="display: inline;"><strong>impact</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping/impact" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>Impact is a measure of the effect of an incident, problem, or change on business processes.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping/priority"></div>
      <p style="display: inline;"><strong>priority</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping/priority" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>Priority is based on impact and urgency, and it identifies how quickly the service desk should address the task.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping/risk"></div>
      <p style="display: inline;"><strong>risk</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping/risk" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The risk level for the change.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping/state"></div>
      <p style="display: inline;"><strong>state</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping/state" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The state of the change request.</p>
      <p>If <em>state</em> value is <code class='docutils literal notranslate'>assess</code> or <code class='docutils literal notranslate'>authorize</code> or <code class='docutils literal notranslate'>scheduled</code> or <code class='docutils literal notranslate'>implement</code> or <code class='docutils literal notranslate'>review</code> or <code class='docutils literal notranslate'>closed</code>, <em>assignment_group</em> parameter must be filled in. In case that any field is renamed, that check is not performed there.</p>
      <p>For more information on state model and transition, refer to the ServiceNow documentation at <a href='https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_ChangeStateModel.html'>https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_ChangeStateModel.html</a></p>
      <p>Special value that can not be overridden is <code class='docutils literal notranslate'>absent</code>, which would remove a change request from ServiceNow.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-change_request_mapping/urgency"></div>
      <p style="display: inline;"><strong>urgency</strong></p>
      <a class="ansibleOptionLink" href="#parameter-change_request_mapping/urgency" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The extent to which resolution of an change request can bear delay.</p>
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

* `servicenow.itsm.change\_request <change_request_module.rst>`__

  Manage ServiceNow change requests.

Examples
--------

.. code-block:: yaml

    - name: Retrieve all change requests
      servicenow.itsm.change_request_info:
      register: result

    - name: Retrieve a specific change request by its sys_id
      servicenow.itsm.change_request_info:
        sys_id: 471bfbc7a9fe198101e77a3e10e5d47f
      register: result

    - name: Retrieve change requests by number
      servicenow.itsm.change_request_info:
        number: PRB0007601
      register: result

    - name: Retrieve change requests that contain SAP in its short description by using field query
      servicenow.itsm.change_request_info:
        query:
          - short_description: LIKE SAP
      register: result

    - name: Retrieve change requests that contain SAP in its short description by using field sysparm_query
      servicenow.itsm.change_request_info:
        sysparm_query: short_descriptionLIKESAP
      register: result

    - name: Retrieve new change requests assigned to abel.tuter or bertie.luby
      servicenow.itsm.change_request_info:
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
      <p>A list of change request records.</p>
      <p style="margin-top: 8px;"><b>Returned:</b> success</p>
      <p style="margin-top: 8px; color: blue; word-wrap: break-word; word-break: break-all;"><b style="color: black;">Sample:</b> <code>[{&#34;active&#34;: &#34;false&#34;, &#34;activity_due&#34;: &#34;&#34;, &#34;additional_assignee_list&#34;: &#34;&#34;, &#34;approval&#34;: &#34;approved&#34;, &#34;approval_history&#34;: &#34;&#34;, &#34;approval_set&#34;: &#34;&#34;, &#34;assigned_to&#34;: &#34;&#34;, &#34;assignment_group&#34;: &#34;d625dccec0a8016700a222a0f7900d06&#34;, &#34;attachments&#34;: [{&#34;average_image_color&#34;: &#34;&#34;, &#34;chunk_size_bytes&#34;: &#34;700000&#34;, &#34;compressed&#34;: &#34;true&#34;, &#34;content_type&#34;: &#34;text/plain&#34;, &#34;download_link&#34;: &#34;https://www.example.com/api/now/attachment/5f7d3c950706301022f9ffa08c1ed062/file&#34;, &#34;file_name&#34;: &#34;sample_file1.txt&#34;, &#34;hash&#34;: &#34;6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e&#34;, &#34;image_height&#34;: &#34;&#34;, &#34;image_width&#34;: &#34;&#34;, &#34;size_bytes&#34;: &#34;210&#34;, &#34;size_compressed&#34;: &#34;206&#34;, &#34;state&#34;: &#34;pending&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2021-08-17 11:18:33&#34;, &#34;sys_id&#34;: &#34;5f7d3c950706301022f9ffa08c1ed062&#34;, &#34;sys_mod_count&#34;: &#34;0&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2021-08-17 11:18:33&#34;, &#34;table_name&#34;: &#34;change_request&#34;, &#34;table_sys_id&#34;: &#34;3a7db0d50706301022f9ffa08c1ed092&#34;}], &#34;backout_plan&#34;: &#34;&#34;, &#34;business_duration&#34;: &#34;&#34;, &#34;business_service&#34;: &#34;&#34;, &#34;cab_date&#34;: &#34;&#34;, &#34;cab_delegate&#34;: &#34;&#34;, &#34;cab_recommendation&#34;: &#34;&#34;, &#34;cab_required&#34;: &#34;false&#34;, &#34;calendar_duration&#34;: &#34;&#34;, &#34;category&#34;: &#34;Other&#34;, &#34;change_plan&#34;: &#34;&#34;, &#34;close_code&#34;: &#34;successful&#34;, &#34;close_notes&#34;: &#34;Completed successfully&#34;, &#34;closed_at&#34;: &#34;2015-07-06 18:18:53&#34;, &#34;closed_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;cmdb_ci&#34;: &#34;&#34;, &#34;comments&#34;: &#34;&#34;, &#34;comments_and_work_notes&#34;: &#34;&#34;, &#34;company&#34;: &#34;&#34;, &#34;conflict_last_run&#34;: &#34;&#34;, &#34;conflict_status&#34;: &#34;Not Run&#34;, &#34;contact_type&#34;: &#34;phone&#34;, &#34;contract&#34;: &#34;&#34;, &#34;correlation_display&#34;: &#34;&#34;, &#34;correlation_id&#34;: &#34;&#34;, &#34;delivery_plan&#34;: &#34;&#34;, &#34;delivery_task&#34;: &#34;&#34;, &#34;description&#34;: &#34;Decommission a server&#34;, &#34;due_date&#34;: &#34;&#34;, &#34;end_date&#34;: &#34;&#34;, &#34;escalation&#34;: &#34;0&#34;, &#34;expected_start&#34;: &#34;&#34;, &#34;follow_up&#34;: &#34;&#34;, &#34;group_list&#34;: &#34;&#34;, &#34;impact&#34;: &#34;3&#34;, &#34;implementation_plan&#34;: &#34;Implementation plan&#34;, &#34;justification&#34;: &#34;&#34;, &#34;knowledge&#34;: &#34;false&#34;, &#34;location&#34;: &#34;&#34;, &#34;made_sla&#34;: &#34;true&#34;, &#34;number&#34;: &#34;CHG0000023&#34;, &#34;on_hold&#34;: &#34;false&#34;, &#34;on_hold_reason&#34;: &#34;&#34;, &#34;on_hold_task&#34;: &#34;&#34;, &#34;opened_at&#34;: &#34;2015-07-06 18:17:21&#34;, &#34;opened_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;order&#34;: &#34;&#34;, &#34;outside_maintenance_schedule&#34;: &#34;false&#34;, &#34;parent&#34;: &#34;&#34;, &#34;phase&#34;: &#34;requested&#34;, &#34;phase_state&#34;: &#34;open&#34;, &#34;priority&#34;: &#34;4&#34;, &#34;production_system&#34;: &#34;false&#34;, &#34;reason&#34;: &#34;&#34;, &#34;reassignment_count&#34;: &#34;2&#34;, &#34;requested_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;requested_by_date&#34;: &#34;&#34;, &#34;review_comments&#34;: &#34;&#34;, &#34;review_date&#34;: &#34;&#34;, &#34;review_status&#34;: &#34;&#34;, &#34;risk&#34;: &#34;3&#34;, &#34;risk_impact_analysis&#34;: &#34;&#34;, &#34;route_reason&#34;: &#34;&#34;, &#34;scope&#34;: &#34;3&#34;, &#34;service_offering&#34;: &#34;&#34;, &#34;short_description&#34;: &#34;Decommission server&#34;, &#34;sla_due&#34;: &#34;&#34;, &#34;start_date&#34;: &#34;&#34;, &#34;state&#34;: &#34;3&#34;, &#34;std_change_producer_version&#34;: &#34;deb8544047810200e90d87e8dee490af&#34;, &#34;sys_class_name&#34;: &#34;change_request&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2015-07-06 18:17:22&#34;, &#34;sys_domain&#34;: &#34;global&#34;, &#34;sys_domain_path&#34;: &#34;/&#34;, &#34;sys_id&#34;: &#34;70ad699e47410200e90d87e8dee4907d&#34;, &#34;sys_mod_count&#34;: &#34;8&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2015-07-06 18:18:53&#34;, &#34;task_effective_number&#34;: &#34;CHG0000023&#34;, &#34;test_plan&#34;: &#34;Test plan&#34;, &#34;time_worked&#34;: &#34;&#34;, &#34;type&#34;: &#34;standard&#34;, &#34;unauthorized&#34;: &#34;false&#34;, &#34;universal_request&#34;: &#34;&#34;, &#34;upon_approval&#34;: &#34;proceed&#34;, &#34;upon_reject&#34;: &#34;cancel&#34;, &#34;urgency&#34;: &#34;3&#34;, &#34;user_input&#34;: &#34;&#34;, &#34;watch_list&#34;: &#34;&#34;, &#34;work_end&#34;: &#34;2015-07-06 18:18:34&#34;, &#34;work_notes&#34;: &#34;&#34;, &#34;work_notes_list&#34;: &#34;&#34;, &#34;work_start&#34;: &#34;2015-07-06 18:17:41&#34;}]</code></p>
    </td>
  </tr>
  </tbody>
  </table>




Authors
~~~~~~~

- Manca Bizjak (@mancabizjak)
- Miha Dolinar (@mdolin)
- Tadej Borovsak (@tadeboro)
- Matej Pevec (@mysteriouswolf)



Collection links
~~~~~~~~~~~~~~~~

* `Issue Tracker <https://github.com/ansible-collections/servicenow.itsm/issues>`__
* `Repository (Sources) <https://github.com/ansible-collections/servicenow.itsm>`__
