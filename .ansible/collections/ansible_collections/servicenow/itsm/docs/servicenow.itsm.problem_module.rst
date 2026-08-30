.. Created with antsibull-docs 2.14.0

servicenow.itsm.problem module -- Manage ServiceNow problems
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.problem``.

New in servicenow.itsm 1.0.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Create, delete or update a ServiceNow problem.
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
      <div class="ansibleOptionAnchor" id="parameter-assigned_to"></div>
      <p style="display: inline;"><strong>assigned_to</strong></p>
      <a class="ansibleOptionLink" href="#parameter-assigned_to" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>A person who will assess this problem.</p>
      <p>Expected value for <em>assigned_to</em> is user id (usually in the form of <code class='docutils literal notranslate'>first_name.last_name</code>).</p>
      <p>This field is required when creating new problems for all problem <em>state</em>s except <code class='docutils literal notranslate'>new</code>.</p>
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
      <div class="ansibleOptionAnchor" id="parameter-base_api_path"></div>
      <p style="display: inline;"><strong>base_api_path</strong></p>
      <a class="ansibleOptionLink" href="#parameter-base_api_path" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.0.0</i></p>
    </td>
    <td valign="top">
      <p>Base API path for the ServiceNow problem state management scripted API.</p>
      <p>Used for managing problem state transitions.</p>
      <p>Requires <em>API for Red Hat Ansible Automation Platform Certified Content Collection</em> application to be installed from the ServiceNow Store <a href='https://store.servicenow.com/sn_appstore_store.do#!/store/application/9b33c83a1bcc5510b76a0d0fdc4bcb21/1.0.0?sl=sh'>https://store.servicenow.com/sn_appstore_store.do#!/store/application/9b33c83a1bcc5510b76a0d0fdc4bcb21/1.0.0?sl=sh</a>.</p>
      <p>Considered mostly for development and testing purposes, as in most cases the default value should be fine.</p>
      <p>Starting with release <em>Rome</em>, <em>ServiceNow Table API</em> no longer supports problem state transitions, which is worked around by using this server-side scripted REST API resource.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;/api/x_rhtpp_ansible/problem&#34;</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-cause_notes"></div>
      <p style="display: inline;"><strong>cause_notes</strong></p>
      <a class="ansibleOptionLink" href="#parameter-cause_notes" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Provide information on what caused the problem.</p>
      <p>Required if <em>state</em> is <code class='docutils literal notranslate'>in_progress</code>.</p>
      <p>Required if <em>state</em> is <code class='docutils literal notranslate'>resolved</code> or <code class='docutils literal notranslate'>closed</code> and <em>resolution_code</em> is <code class='docutils literal notranslate'>fix_applied</code> or <code class='docutils literal notranslate'>risk_accepted</code>.</p>
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
      <p>The reason for closing the problem.</p>
      <p>Required if <em>state</em> is <code class='docutils literal notranslate'>resolved</code> or <code class='docutils literal notranslate'>closed</code> and <em>resolution_code</em> is <code class='docutils literal notranslate'>risk_accepted</code> or <code class='docutils literal notranslate'>canceled</code>.</p>
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
      <p>Detailed description of the problem.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-duplicate_of"></div>
      <p style="display: inline;"><strong>duplicate_of</strong></p>
      <a class="ansibleOptionLink" href="#parameter-duplicate_of" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Number of the problem of which this problem is a duplicate of.</p>
      <p>Required if <em>state</em> is <code class='docutils literal notranslate'>resolved</code> or <code class='docutils literal notranslate'>closed</code> and <em>resolution_code</em> is <code class='docutils literal notranslate'>duplicate</code>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-fix_notes"></div>
      <p style="display: inline;"><strong>fix_notes</strong></p>
      <a class="ansibleOptionLink" href="#parameter-fix_notes" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>Notes on how the problem was fixed.</p>
      <p>Required if <em>state</em> is <code class='docutils literal notranslate'>in_progress</code>.</p>
      <p>Required if <em>state</em> is <code class='docutils literal notranslate'>resolved</code> or <code class='docutils literal notranslate'>closed</code> and <em>resolution_code</em> is <code class='docutils literal notranslate'>fix_applied</code>.</p>
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
      <p>Effect that the problem has on business.</p>
      <p>Default choices are <code class='docutils literal notranslate'>low</code>, <code class='docutils literal notranslate'>medium</code>, <code class='docutils literal notranslate'>high</code>. One can override them by setting <em>problem_mapping.impact</em>.</p>
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
      <div class="ansibleOptionAnchor" id="parameter-other"></div>
      <p style="display: inline;"><strong>other</strong></p>
      <a class="ansibleOptionLink" href="#parameter-other" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>Optional remaining parameters.</p>
      <p>For more information on optional parameters, refer to the ServiceNow documentation on creating problems at <a href='https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/task/create-a-problem-v2.html'>https://docs.servicenow.com/bundle/tokyo-it-service-management/page/product/problem-management/task/create-a-problem-v2.html</a>.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-problem_mapping"></div>
      <p style="display: inline;"><strong>problem_mapping</strong></p>
      <a class="ansibleOptionLink" href="#parameter-problem_mapping" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.3.0</i></p>
    </td>
    <td valign="top">
      <p>User mapping for <em>Problem</em> object, where user can override Choice Lists values for objects.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-problem_mapping/impact"></div>
      <p style="display: inline;"><strong>impact</strong></p>
      <a class="ansibleOptionLink" href="#parameter-problem_mapping/impact" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>Effect that the problem has on business.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-problem_mapping/problem_state"></div>
      <p style="display: inline;"><strong>problem_state</strong></p>
      <a class="ansibleOptionLink" href="#parameter-problem_mapping/problem_state" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>State of the problem.</p>
      <p>If a problem does not yet exist, all states except for <code class='docutils literal notranslate'>new</code> require setting of <em>assigned_to</em> parameter.</p>
      <p>This mapping can also be edited inside Choice Lists inside ServiceNow and can differ from state mapping.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-problem_mapping/state"></div>
      <p style="display: inline;"><strong>state</strong></p>
      <a class="ansibleOptionLink" href="#parameter-problem_mapping/state" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>State of the problem.</p>
      <p>If a problem does not yet exist, all states except for <code class='docutils literal notranslate'>new</code> require setting of <em>assigned_to</em> parameter.</p>
      <p>Special value that can not be overridden is <code class='docutils literal notranslate'>absent</code>, which would remove a problem from ServiceNow.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-problem_mapping/urgency"></div>
      <p style="display: inline;"><strong>urgency</strong></p>
      <a class="ansibleOptionLink" href="#parameter-problem_mapping/urgency" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The extent to which the problem resolution can bear delay.</p>
    </td>
  </tr>

  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-resolution_code"></div>
      <p style="display: inline;"><strong>resolution_code</strong></p>
      <a class="ansibleOptionLink" href="#parameter-resolution_code" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The reason for problem resolution.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;fix_applied&#34;</code></p></li>
        <li><p><code>&#34;risk_accepted&#34;</code></p></li>
        <li><p><code>&#34;duplicate&#34;</code></p></li>
        <li><p><code>&#34;canceled&#34;</code></p></li>
      </ul>

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
      <p>Short description of the problem that the problem-solving team should address.</p>
      <p>Required if the problem does not exist yet.</p>
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
      <p>State of the problem.</p>
      <p>If a problem does not yet exist, all states except for <code class='docutils literal notranslate'>new</code> require setting of <em>assigned_to</em> parameter.</p>
      <p>Default choices are <code class='docutils literal notranslate'>new</code>, <code class='docutils literal notranslate'>assess</code>, <code class='docutils literal notranslate'>root_cause_analysis</code>, <code class='docutils literal notranslate'>fix_in_progress</code>, <code class='docutils literal notranslate'>resolved</code>, <code class='docutils literal notranslate'>closed</code>, <code class='docutils literal notranslate'>absent</code>. One can override them by setting <em>problem_mapping.state</em>.</p>
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
      <div class="ansibleOptionAnchor" id="parameter-urgency"></div>
      <p style="display: inline;"><strong>urgency</strong></p>
      <a class="ansibleOptionLink" href="#parameter-urgency" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>The extent to which the problem resolution can bear delay.</p>
      <p>Default choices are <code class='docutils literal notranslate'>low</code>, <code class='docutils literal notranslate'>medium</code>, <code class='docutils literal notranslate'>high</code>. One can override them by setting <em>problem_mapping.urgency</em>.</p>
    </td>
  </tr>
  </tbody>
  </table>





See Also
--------

* `servicenow.itsm.problem\_info <problem_info_module.rst>`__

  List ServiceNow problems.
* `servicenow.itsm.problem\_task <problem_task_module.rst>`__

  Manage ServiceNow problem tasks.
* `servicenow.itsm.problem\_task\_info <problem_task_info_module.rst>`__

  List ServiceNow problem tasks.

Examples
--------

.. code-block:: yaml

    - name: Create a problem
      servicenow.itsm.problem:
        state: new
        short_description: Issue with the network printer
        description: Since this morning, all printer jobs are stuck.
        attachments:
          - path: path/to/attachment.txt
        impact: medium
        urgency: low
        other:
          user_input: notes

    - name: Assign a problem to a user for assessment
      servicenow.itsm.problem:
        number: PRB0000010
        state: assess
        assigned_to: problem.manager

    - name: Mark a problem for root cause analysis
      servicenow.itsm.problem:
        number: PRB0000010
        state: root_cause_analysis

    - name: Work on fixing a problem
      servicenow.itsm.problem:
        number: PRB0000010
        state: fix_in_progress
        cause_notes: I identified the issue.
        fix_notes: Fix here.


    - name: Close a problem as fixed
      servicenow.itsm.problem:
        number: PRB0000010
        state: closed
        resolution_code: fix_applied
        cause_notes: I found that this doesn't work.
        fix_notes: I solved it like this.

    - name: Close a problem as duplicate
      servicenow.itsm.problem:
        number: PRB0000010
        state: closed
        resolution_code: duplicate
        duplicate_of: PRB0000001

    - name: Cancel a problem
      servicenow.itsm.problem:
        number: PRB0000010
        state: closed
        resolution_code: canceled
        close_notes: The problem seems to have resolved itself.

    - name: Delete a problem
      servicenow.itsm.problem:
        number: PRB0000010
        state: absent




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
      <p>The problem record.</p>
      <p style="margin-top: 8px;"><b>Returned:</b> success</p>
      <p style="margin-top: 8px; color: blue; word-wrap: break-word; word-break: break-all;"><b style="color: black;">Sample:</b> <code>{&#34;active&#34;: &#34;true&#34;, &#34;activity_due&#34;: &#34;&#34;, &#34;additional_assignee_list&#34;: &#34;&#34;, &#34;approval&#34;: &#34;not requested&#34;, &#34;approval_history&#34;: &#34;&#34;, &#34;approval_set&#34;: &#34;&#34;, &#34;assigned_to&#34;: &#34;73ab3f173b331300ad3cc9bb34efc4df&#34;, &#34;assignment_group&#34;: &#34;&#34;, &#34;attachments&#34;: [{&#34;average_image_color&#34;: &#34;&#34;, &#34;chunk_size_bytes&#34;: &#34;700000&#34;, &#34;compressed&#34;: &#34;true&#34;, &#34;content_type&#34;: &#34;text/plain&#34;, &#34;download_link&#34;: &#34;https://www.example.com/api/now/attachment/31cdf4d50706301022f9ffa08c1ed07f/file&#34;, &#34;file_name&#34;: &#34;sample_file1.txt&#34;, &#34;hash&#34;: &#34;6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e&#34;, &#34;image_height&#34;: &#34;&#34;, &#34;image_width&#34;: &#34;&#34;, &#34;size_bytes&#34;: &#34;210&#34;, &#34;size_compressed&#34;: &#34;206&#34;, &#34;state&#34;: &#34;pending&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2021-08-17 11:19:49&#34;, &#34;sys_id&#34;: &#34;31cdf4d50706301022f9ffa08c1ed07f&#34;, &#34;sys_mod_count&#34;: &#34;0&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2021-08-17 11:19:49&#34;, &#34;table_name&#34;: &#34;problem&#34;, &#34;table_sys_id&#34;: &#34;6dcdb4d50706301022f9ffa08c1ed0fb&#34;}], &#34;business_duration&#34;: &#34;&#34;, &#34;business_service&#34;: &#34;&#34;, &#34;calendar_duration&#34;: &#34;&#34;, &#34;category&#34;: &#34;software&#34;, &#34;cause_notes&#34;: &#34;&#34;, &#34;close_notes&#34;: &#34;&#34;, &#34;closed_at&#34;: &#34;&#34;, &#34;closed_by&#34;: &#34;&#34;, &#34;cmdb_ci&#34;: &#34;27d32778c0a8000b00db970eeaa60f16&#34;, &#34;comments&#34;: &#34;&#34;, &#34;comments_and_work_notes&#34;: &#34;&#34;, &#34;company&#34;: &#34;&#34;, &#34;confirmed_at&#34;: &#34;&#34;, &#34;confirmed_by&#34;: &#34;&#34;, &#34;contact_type&#34;: &#34;&#34;, &#34;contract&#34;: &#34;&#34;, &#34;correlation_display&#34;: &#34;&#34;, &#34;correlation_id&#34;: &#34;&#34;, &#34;delivery_plan&#34;: &#34;&#34;, &#34;delivery_task&#34;: &#34;&#34;, &#34;description&#34;: &#34;Unable to send or receive emails.&#34;, &#34;due_date&#34;: &#34;&#34;, &#34;duplicate_of&#34;: &#34;&#34;, &#34;escalation&#34;: &#34;0&#34;, &#34;expected_start&#34;: &#34;&#34;, &#34;first_reported_by_task&#34;: &#34;&#34;, &#34;fix_communicated_at&#34;: &#34;&#34;, &#34;fix_communicated_by&#34;: &#34;&#34;, &#34;fix_notes&#34;: &#34;&#34;, &#34;follow_up&#34;: &#34;&#34;, &#34;group_list&#34;: &#34;&#34;, &#34;impact&#34;: &#34;low&#34;, &#34;knowledge&#34;: &#34;false&#34;, &#34;known_error&#34;: &#34;false&#34;, &#34;location&#34;: &#34;&#34;, &#34;made_sla&#34;: &#34;true&#34;, &#34;major_problem&#34;: &#34;false&#34;, &#34;number&#34;: &#34;PRB0007601&#34;, &#34;opened_at&#34;: &#34;2018-08-30 08:08:39&#34;, &#34;opened_by&#34;: &#34;6816f79cc0a8016401c5a33be04be441&#34;, &#34;order&#34;: &#34;&#34;, &#34;parent&#34;: &#34;&#34;, &#34;priority&#34;: &#34;5&#34;, &#34;problem_state&#34;: &#34;new&#34;, &#34;reassignment_count&#34;: &#34;0&#34;, &#34;related_incidents&#34;: &#34;0&#34;, &#34;reopen_count&#34;: &#34;0&#34;, &#34;reopened_at&#34;: &#34;&#34;, &#34;reopened_by&#34;: &#34;&#34;, &#34;resolution_code&#34;: &#34;&#34;, &#34;resolved_at&#34;: &#34;&#34;, &#34;resolved_by&#34;: &#34;&#34;, &#34;review_outcome&#34;: &#34;&#34;, &#34;rfc&#34;: &#34;&#34;, &#34;route_reason&#34;: &#34;&#34;, &#34;service_offering&#34;: &#34;&#34;, &#34;short_description&#34;: &#34;Unable to send or receive emails.&#34;, &#34;sla_due&#34;: &#34;&#34;, &#34;state&#34;: &#34;new&#34;, &#34;subcategory&#34;: &#34;email&#34;, &#34;sys_class_name&#34;: &#34;problem&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2018-08-30 08:09:05&#34;, &#34;sys_domain&#34;: &#34;global&#34;, &#34;sys_domain_path&#34;: &#34;/&#34;, &#34;sys_id&#34;: &#34;62304320731823002728660c4cf6a7e8&#34;, &#34;sys_mod_count&#34;: &#34;1&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2018-12-12 07:16:57&#34;, &#34;task_effective_number&#34;: &#34;PRB0007601&#34;, &#34;time_worked&#34;: &#34;&#34;, &#34;universal_request&#34;: &#34;&#34;, &#34;upon_approval&#34;: &#34;proceed&#34;, &#34;upon_reject&#34;: &#34;cancel&#34;, &#34;urgency&#34;: &#34;low&#34;, &#34;user_input&#34;: &#34;&#34;, &#34;watch_list&#34;: &#34;&#34;, &#34;work_end&#34;: &#34;&#34;, &#34;work_notes&#34;: &#34;&#34;, &#34;work_notes_list&#34;: &#34;&#34;, &#34;work_start&#34;: &#34;&#34;, &#34;workaround&#34;: &#34;&#34;, &#34;workaround_applied&#34;: &#34;false&#34;, &#34;workaround_communicated_at&#34;: &#34;&#34;, &#34;workaround_communicated_by&#34;: &#34;&#34;}</code></p>
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
- Uros Pascinski (@uscinski)



Collection links
~~~~~~~~~~~~~~~~

* `Issue Tracker <https://github.com/ansible-collections/servicenow.itsm/issues>`__
* `Repository (Sources) <https://github.com/ansible-collections/servicenow.itsm>`__
