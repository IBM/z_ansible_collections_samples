.. Created with antsibull-docs 2.14.0

servicenow.itsm.change_request module -- Manage ServiceNow change requests
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.change_request``.

New in servicenow.itsm 1.0.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Create, delete or update a ServiceNow change request.
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
      <div class="ansibleOptionAnchor" id="parameter-assignment_group"></div>
      <p style="display: inline;"><strong>assignment_group</strong></p>
      <a class="ansibleOptionLink" href="#parameter-assignment_group" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The name of the group that the change request is assigned to.</p>
      <p>Required if <em>state</em> value is <code class='docutils literal notranslate'>assess</code> or <code class='docutils literal notranslate'>authorize</code> or <code class='docutils literal notranslate'>scheduled</code> or <code class='docutils literal notranslate'>implement</code> or <code class='docutils literal notranslate'>review</code> or <code class='docutils literal notranslate'>closed</code>.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>assignment_group_id</code>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-assignment_group_id"></div>
      <p style="display: inline;"><strong>assignment_group_id</strong></p>
      <a class="ansibleOptionLink" href="#parameter-assignment_group_id" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.4.0</i></p>
    </td>
    <td valign="top">
      <p>The sys_id of the group that the change request is assigned to.</p>
      <p>Required if <em>state</em> value is <code class='docutils literal notranslate'>assess</code> or <code class='docutils literal notranslate'>authorize</code> or <code class='docutils literal notranslate'>scheduled</code> or <code class='docutils literal notranslate'>implement</code> or <code class='docutils literal notranslate'>review</code> or <code class='docutils literal notranslate'>closed</code>.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>assignment_group</code>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-attachments"></div>
      <p style="display: inline;"><strong>attachments</strong></p>
      <a class="ansibleOptionLink" href="#parameter-attachments" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=dictionary</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.2.0</i></p>
    </td>
    <td valign="top">
      <p>ServiceNow attachments.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-attachments/name"></div>
      <p style="display: inline;"><strong>name</strong></p>
      <a class="ansibleOptionLink" href="#parameter-attachments/name" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Name of the file to be uploaded.</p>
      <p>Serves as unique identifier.</p>
      <p>If not specified, the module will use <em>path</em>&#x27;s base name.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-attachments/path"></div>
      <p style="display: inline;"><strong>path</strong></p>
      <a class="ansibleOptionLink" href="#parameter-attachments/path" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>Path to the file to be uploaded.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-attachments/type"></div>
      <p style="display: inline;"><strong>type</strong></p>
      <a class="ansibleOptionLink" href="#parameter-attachments/type" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>MIME type of the file to be attached.</p>
      <p>If not specified, the module will try to guess the file&#x27;s type from its extension.</p>
    </td>
  </tr>

  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-category"></div>
      <p style="display: inline;"><strong>category</strong></p>
      <a class="ansibleOptionLink" href="#parameter-category" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The category of the change request.</p>
      <p>Default choices are <code class='docutils literal notranslate'>hardware</code>, <code class='docutils literal notranslate'>software</code>, <code class='docutils literal notranslate'>service</code>, <code class='docutils literal notranslate'>system_software</code>, <code class='docutils literal notranslate'>aplication_software</code>, <code class='docutils literal notranslate'>network</code>, <code class='docutils literal notranslate'>telecom</code>, <code class='docutils literal notranslate'>documentation</code>, <code class='docutils literal notranslate'>other</code>.</p>
      <p>One can override them by setting <em>change_request_mapping.category</em>.</p>
      <p>Hard-coded choices for <code class='docutils literal notranslate'>category</code> were removed in 2.5.0.</p>
    </td>
  </tr>
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
      <div class="ansibleOptionAnchor" id="parameter-close_code"></div>
      <p style="display: inline;"><strong>close_code</strong></p>
      <a class="ansibleOptionLink" href="#parameter-close_code" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Provide information on how the change request was resolved.</p>
      <p>The change request must have this parameter set prior to transitioning to the <code class='docutils literal notranslate'>closed</code> state.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;successful&#34;</code></p></li>
        <li><p><code>&#34;successful_issues&#34;</code></p></li>
        <li><p><code>&#34;unsuccessful&#34;</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-close_notes"></div>
      <p style="display: inline;"><strong>close_notes</strong></p>
      <a class="ansibleOptionLink" href="#parameter-close_notes" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Resolution notes added by the user who closed the change request.</p>
      <p>The change request must have this parameter set prior to transitioning to the <code class='docutils literal notranslate'>closed</code> state.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-description"></div>
      <p style="display: inline;"><strong>description</strong></p>
      <a class="ansibleOptionLink" href="#parameter-description" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>A detailed description of the change request.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-hold_reason"></div>
      <p style="display: inline;"><strong>hold_reason</strong></p>
      <a class="ansibleOptionLink" href="#parameter-hold_reason" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Reason why change request is on hold.</p>
      <p>Required if change request&#x27;s <em>on_hold</em> value will be <code class='docutils literal notranslate'>true</code>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-impact"></div>
      <p style="display: inline;"><strong>impact</strong></p>
      <a class="ansibleOptionLink" href="#parameter-impact" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Impact is a measure of the effect of an incident, problem, or change on business processes.</p>
      <p>Default choices are <code class='docutils literal notranslate'>high</code>, <code class='docutils literal notranslate'>medium</code>, <code class='docutils literal notranslate'>low</code>. One can override them by setting <em>change_request_mapping.impact</em>.</p>
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
      <p>Number of the record to operate on.</p>
      <p>Note that contrary to <em>sys_id</em>, <em>number</em> may not uniquely identify a record.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-on_hold"></div>
      <p style="display: inline;"><strong>on_hold</strong></p>
      <a class="ansibleOptionLink" href="#parameter-on_hold" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
    </td>
    <td valign="top">
      <p>A change request can be put on hold when <em>state</em> is not in the <code class='docutils literal notranslate'>new</code>, <code class='docutils literal notranslate'>canceled</code>, or <code class='docutils literal notranslate'>closed</code>.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>false</code></p></li>
        <li><p><code>true</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-other"></div>
      <p style="display: inline;"><strong>other</strong></p>
      <a class="ansibleOptionLink" href="#parameter-other" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>Optional remaining parameters.</p>
      <p>For more information on optional parameters, refer to the ServiceNow change request documentation at <a href='https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/task/t_CreateAChange.html'>https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/task/t_CreateAChange.html</a>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-priority"></div>
      <p style="display: inline;"><strong>priority</strong></p>
      <a class="ansibleOptionLink" href="#parameter-priority" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Priority is based on impact and urgency, and it identifies how quickly the service desk should address the task.</p>
      <p>Default choices are <code class='docutils literal notranslate'>critical</code>, <code class='docutils literal notranslate'>high</code>, <code class='docutils literal notranslate'>moderate</code>, <code class='docutils literal notranslate'>low</code>. One can override them by setting <em>change_request_mapping.priority</em>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-requested_by"></div>
      <p style="display: inline;"><strong>requested_by</strong></p>
      <a class="ansibleOptionLink" href="#parameter-requested_by" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>User who requested the change.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-risk"></div>
      <p style="display: inline;"><strong>risk</strong></p>
      <a class="ansibleOptionLink" href="#parameter-risk" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The risk level for the change.</p>
      <p>Default choices are <code class='docutils literal notranslate'>high</code>, <code class='docutils literal notranslate'>moderate</code>, <code class='docutils literal notranslate'>low</code>. One can override them by setting <em>change_request_mapping.risk</em>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-short_description"></div>
      <p style="display: inline;"><strong>short_description</strong></p>
      <a class="ansibleOptionLink" href="#parameter-short_description" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>A summary of the change request.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-state"></div>
      <p style="display: inline;"><strong>state</strong></p>
      <a class="ansibleOptionLink" href="#parameter-state" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The state of the change request.</p>
      <p>If <em>state</em> value is <code class='docutils literal notranslate'>assess</code> or <code class='docutils literal notranslate'>authorize</code> or <code class='docutils literal notranslate'>scheduled</code> or <code class='docutils literal notranslate'>implement</code> or <code class='docutils literal notranslate'>review</code> or <code class='docutils literal notranslate'>closed</code>, <em>assignment_group</em> parameter must be filled in.</p>
      <p>For more information on state model and transition, refere to the ServiceNow documentation at <a href='https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_ChangeStateModel.html'>https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_ChangeStateModel.html</a></p>
      <p>Default choices are <code class='docutils literal notranslate'>new</code>, <code class='docutils literal notranslate'>assess</code>, <code class='docutils literal notranslate'>authorize</code>, <code class='docutils literal notranslate'>scheduled</code>, <code class='docutils literal notranslate'>implement</code>, <code class='docutils literal notranslate'>review</code>, <code class='docutils literal notranslate'>closed</code>, <code class='docutils literal notranslate'>canceled</code>, <code class='docutils literal notranslate'>absent</code>. One can override them by setting <em>change_request_mapping.state</em>.</p>
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
      <p>Unique identifier of the record to operate on.</p>
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
      <p>Predefined template name for standard change request.</p>
      <p>For more information on templates refer to ServiceNow documentation at <a href='https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_StandardChangeCatalogPlugin.html'>https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/change-management/concept/c_StandardChangeCatalogPlugin.html</a> or find template names on &lt;your_service_id&gt;.service-now.com/nav_to.do?uri=%2Fstd_change_producer_version_list.do%3F</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-type"></div>
      <p style="display: inline;"><strong>type</strong></p>
      <a class="ansibleOptionLink" href="#parameter-type" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Specify what type of change is required.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;standard&#34;</code></p></li>
        <li><p><code>&#34;normal&#34;</code></p></li>
        <li><p><code>&#34;emergency&#34;</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-urgency"></div>
      <p style="display: inline;"><strong>urgency</strong></p>
      <a class="ansibleOptionLink" href="#parameter-urgency" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The extent to which resolution of an change request can bear delay.</p>
      <p>Default choices are <code class='docutils literal notranslate'>high</code>, <code class='docutils literal notranslate'>medium</code>, <code class='docutils literal notranslate'>low</code>. One can override them by setting <em>change_request_mapping.urgency</em>.</p>
    </td>
  </tr>
  </tbody>
  </table>





See Also
--------

* `servicenow.itsm.change\_request\_info <change_request_info_module.rst>`__

  List ServiceNow change requests.

Examples
--------

.. code-block:: yaml

    - name: Create change request
      servicenow.itsm.change_request:
        instance:
          host: https://instance_id.service-now.com
          username: user
          password: pass

        type: standard
        state: new
        requested_by: some.user
        short_description: Install new Cisco
        description: Please install new Cat. 6500 in Data center 01
        attachments:
          - path: path/to/attachment.txt
        priority: moderate
        risk: low
        impact: low

        other:
          expected_start: 2021-02-12

    - name: Change state of the change request
      servicenow.itsm.change_request:
        instance:
          host: https://instance_id.service-now.com
          username: user
          password: pass

        state: assess
        assignment_group: some.group
        number: CHG0000001

    - name: Close change_request
      servicenow.itsm.change_request:
        instance:
          host: https://instance_id.service-now.com
          username: user
          password: pass

        state: closed
        close_code: "successful"
        close_notes: "Closed"
        assignment_group: some.group
        number: CHG0000001

    - name: Delete change_request
      servicenow.itsm.change_request:
        instance:
          host: https://instance_id.service-now.com
          username: user
          password: pass

        state: absent
        number: CHG0000001






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
