.. Created with antsibull-docs 2.14.0

servicenow.itsm.configuration_item_info module -- List ServiceNow configuration item
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.configuration_item_info``.

New in servicenow.itsm 1.0.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Retrieve information about the ServiceNow configuration items and also attachments related to this CI.
- For more information, refer to the ServiceNow configuration item management documentation at \ `https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/concept/c\_ITILConfigurationManagement.html <https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/concept/c_ITILConfigurationManagement.html>`__.








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
      <div class="ansibleOptionAnchor" id="parameter-configuration_item_mapping"></div>
      <p style="display: inline;"><strong>configuration_item_mapping</strong></p>
      <a class="ansibleOptionLink" href="#parameter-configuration_item_mapping" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.3.0</i></p>
    </td>
    <td valign="top">
      <p>User mappings for <em>Configuration item</em> object.</p>
      <p>Where mapping is not set, the default will be used.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-configuration_item_mapping/environment"></div>
      <p style="display: inline;"><strong>environment</strong></p>
      <a class="ansibleOptionLink" href="#parameter-configuration_item_mapping/environment" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The environment to which this configuration item belongs.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-configuration_item_mapping/install_status"></div>
      <p style="display: inline;"><strong>install_status</strong></p>
      <a class="ansibleOptionLink" href="#parameter-configuration_item_mapping/install_status" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The functional status of the configuration item.</p>
      <p>Special value that can not be overridden is <code class='docutils literal notranslate'>absent</code>, which would remove a configuration item from ServiceNow.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-configuration_item_mapping/operational_status"></div>
      <p style="display: inline;"><strong>operational_status</strong></p>
      <a class="ansibleOptionLink" href="#parameter-configuration_item_mapping/operational_status" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>
    </td>
    <td valign="top">
      <p>The operational status of the configuration item.</p>
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
      <div class="ansibleOptionAnchor" id="parameter-name"></div>
      <p style="display: inline;"><strong>name</strong></p>
      <a class="ansibleOptionLink" href="#parameter-name" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.0.0</i></p>
    </td>
    <td valign="top">
      <p>Unique identifier of the record to retrieve.</p>
      <p>Mutually exclusive with <code class='docutils literal notranslate'>query</code> and <code class='docutils literal notranslate'>sys_id</code>.</p>
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
      <div class="ansibleOptionAnchor" id="parameter-return_fields"></div>
      <p style="display: inline;"><strong>return_fields</strong></p>
      <a class="ansibleOptionLink" href="#parameter-return_fields" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.4.0</i></p>
    </td>
    <td valign="top">
      <p>A list of fields to return.</p>
      <p>If defined you need to add "attachments" as a field to return if you wish also get related attachments data.</p>
      <p>If <code class='docutils literal notranslate'>return_fields</code> is not defined, all fields and also attachments will be returned.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-sys_class_name"></div>
      <p style="display: inline;"><strong>sys_class_name</strong></p>
      <a class="ansibleOptionLink" href="#parameter-sys_class_name" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
    </td>
    <td valign="top">
      <p>ServiceNow configuration item class.</p>
      <p>The value of this parameter should point to a ServiceNow CMDB configuration item table, for instance <code class='docutils literal notranslate'>cmdb_ci_server</code>.</p>
      <p>For a list of valid CMDB tables, refer to ServiceNow documentation on <a href='https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-tables-details.html'>https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-tables-details.html</a>.</p>
      <p>If this parameter is unset when a configuration item info is queried, the default value <code class='docutils literal notranslate'>cmdb_ci</code> will be used.</p>
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

* `servicenow.itsm.configuration\_item <configuration_item_module.rst>`__

  Manage ServiceNow configuration items.

Examples
--------

.. code-block:: yaml

    - name: Retrieve all configuration items
      servicenow.itsm.configuration_item_info:
      register: result

    - name: Retrieve a specific configuration item by sys_id
      servicenow.itsm.configuration_item_info:
        sys_id: 01a9ec0d3790200044e0bfc8bcbe5dc3
      register: result

    - name: Retrieve a specific configuration item by sys_id with limited return fields
      servicenow.itsm.configuration_item_info:
        sys_id: 01a9ec0d3790200044e0bfc8bcbe5dc3
        return_fields:
          - name
          - sys_id
      register: result

    - name: Retrieve a specific configuration item by name
      servicenow.itsm.configuration_item_info:
        name: my-configuration-item
      register: result

    - name: Retrieve all hardware configuration items by using field query
      servicenow.itsm.configuration_item_info:
        query:
          - category: = Hardware
      register: result

    - name: Retrieve all hardware configuration items by using field sysparm_query
      servicenow.itsm.configuration_item_info:
        sysparm_query: category=Hardware
      register: result

    - name: Retrieve configuration items in hardware category assigned to abel.tuter or bertie.luby
      servicenow.itsm.configuration_item_info:
        query:
          - category: = hardware
            assigned_to: = abel.tuter
          - category: = hardware
            assigned_to: = bertie.luby

    - name: Retrieve configuration items using configuration items mapping
      servicenow.itms.configuration_item_info:
        configuration_item_mapping:
          install_status:
            1: installed
          operational_status:
            1: operational




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
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=string</span>
      </p>
    </td>
    <td valign="top">
      <p>A list of configuration item records.</p>
      <p>Note that the fields of the returned records depend on the configuration item&#x27;s <em>sys_class_name</em>.</p>
      <p style="margin-top: 8px;"><b>Returned:</b> success</p>
      <p style="margin-top: 8px; color: blue; word-wrap: break-word; word-break: break-all;"><b style="color: black;">Sample:</b> <code>{&#34;asset&#34;: &#34;05a9ec0d3790200044e0bfc8bcbe5dc2&#34;, &#34;asset_tag&#34;: &#34;P1000440&#34;, &#34;assigned&#34;: &#34;2019-02-28 08:00:00&#34;, &#34;assigned_to&#34;: &#34;8a826bf03710200044e0bfc8bcbe5d96&#34;, &#34;assignment_group&#34;: &#34;&#34;, &#34;attachments&#34;: [{&#34;average_image_color&#34;: &#34;&#34;, &#34;chunk_size_bytes&#34;: &#34;700000&#34;, &#34;compressed&#34;: &#34;true&#34;, &#34;content_type&#34;: &#34;text/plain&#34;, &#34;download_link&#34;: &#34;https://www.example.com/api/now/attachment/919d34d50706301022f9ffa08c1ed047/file&#34;, &#34;file_name&#34;: &#34;sample_file1.txt&#34;, &#34;hash&#34;: &#34;6f2b0dec698566114435a23f15dcac848a40e1fd3e0eda4afe24a663dda23f2e&#34;, &#34;image_height&#34;: &#34;&#34;, &#34;image_width&#34;: &#34;&#34;, &#34;size_bytes&#34;: &#34;210&#34;, &#34;size_compressed&#34;: &#34;206&#34;, &#34;state&#34;: &#34;pending&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2021-08-17 11:18:58&#34;, &#34;sys_id&#34;: &#34;919d34d50706301022f9ffa08c1ed047&#34;, &#34;sys_mod_count&#34;: &#34;0&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;admin&#34;, &#34;sys_updated_on&#34;: &#34;2021-08-17 11:18:58&#34;, &#34;table_name&#34;: &#34;cmdb_ci&#34;, &#34;table_sys_id&#34;: &#34;459d34d50706301022f9ffa08c1ed06a&#34;}], &#34;attestation_score&#34;: &#34;&#34;, &#34;attested&#34;: &#34;false&#34;, &#34;attested_by&#34;: &#34;&#34;, &#34;attested_date&#34;: &#34;&#34;, &#34;attributes&#34;: &#34;&#34;, &#34;can_print&#34;: &#34;false&#34;, &#34;category&#34;: &#34;Hardware&#34;, &#34;change_control&#34;: &#34;&#34;, &#34;checked_in&#34;: &#34;&#34;, &#34;checked_out&#34;: &#34;&#34;, &#34;comments&#34;: &#34;&#34;, &#34;company&#34;: &#34;81fca4cbac1d55eb355b4b6db0e3c80f&#34;, &#34;correlation_id&#34;: &#34;&#34;, &#34;cost&#34;: &#34;1699.99&#34;, &#34;cost_cc&#34;: &#34;USD&#34;, &#34;cost_center&#34;: &#34;d9d01546c0a80a6403e18b82250c80a1&#34;, &#34;delivery_date&#34;: &#34;2018-07-05 07:00:00&#34;, &#34;department&#34;: &#34;a581ab703710200044e0bfc8bcbe5de8&#34;, &#34;discovery_source&#34;: &#34;&#34;, &#34;dns_domain&#34;: &#34;&#34;, &#34;due&#34;: &#34;&#34;, &#34;due_in&#34;: &#34;&#34;, &#34;duplicate_of&#34;: &#34;&#34;, &#34;environment&#34;: &#34;&#34;, &#34;fault_count&#34;: &#34;0&#34;, &#34;first_discovered&#34;: &#34;&#34;, &#34;fqdn&#34;: &#34;&#34;, &#34;gl_account&#34;: &#34;&#34;, &#34;install_date&#34;: &#34;2018-10-02 07:00:00&#34;, &#34;install_status&#34;: &#34;installed&#34;, &#34;invoice_number&#34;: &#34;&#34;, &#34;ip_address&#34;: &#34;&#34;, &#34;justification&#34;: &#34;&#34;, &#34;last_discovered&#34;: &#34;&#34;, &#34;lease_id&#34;: &#34;&#34;, &#34;life_cycle_stage&#34;: &#34;&#34;, &#34;life_cycle_stage_status&#34;: &#34;&#34;, &#34;location&#34;: &#34;8228cda2ac1d55eb7029baf443945c37&#34;, &#34;mac_address&#34;: &#34;&#34;, &#34;maintenance_schedule&#34;: &#34;&#34;, &#34;managed_by&#34;: &#34;&#34;, &#34;managed_by_group&#34;: &#34;&#34;, &#34;manufacturer&#34;: &#34;aa0a6df8c611227601cd2ed45989e0ac&#34;, &#34;model_id&#34;: &#34;0c43b858c611227501522de20c61ac75&#34;, &#34;model_number&#34;: &#34;&#34;, &#34;monitor&#34;: &#34;false&#34;, &#34;name&#34;: &#34;ThinkStation S20&#34;, &#34;operational_status&#34;: &#34;operational&#34;, &#34;order_date&#34;: &#34;2018-06-07 07:00:00&#34;, &#34;owned_by&#34;: &#34;&#34;, &#34;po_number&#34;: &#34;PO100005&#34;, &#34;purchase_date&#34;: &#34;2018-06-22&#34;, &#34;schedule&#34;: &#34;&#34;, &#34;serial_number&#34;: &#34;WCL-206-Q10853-BF&#34;, &#34;short_description&#34;: &#34;&#34;, &#34;skip_sync&#34;: &#34;false&#34;, &#34;start_date&#34;: &#34;&#34;, &#34;subcategory&#34;: &#34;Computer&#34;, &#34;support_group&#34;: &#34;&#34;, &#34;supported_by&#34;: &#34;&#34;, &#34;sys_class_name&#34;: &#34;cmdb_ci_computer&#34;, &#34;sys_class_path&#34;: &#34;/!!/!2/!(&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2012-02-18 08:14:42&#34;, &#34;sys_domain&#34;: &#34;global&#34;, &#34;sys_domain_path&#34;: &#34;/&#34;, &#34;sys_id&#34;: &#34;01a9ec0d3790200044e0bfc8bcbe5dc3&#34;, &#34;sys_mod_count&#34;: &#34;6&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;system&#34;, &#34;sys_updated_on&#34;: &#34;2021-01-16 05:50:31&#34;, &#34;unverified&#34;: &#34;false&#34;, &#34;vendor&#34;: &#34;aa0a6df8c611227601cd2ed45989e0ac&#34;, &#34;warranty_expiration&#34;: &#34;2021-10-01&#34;}</code></p>
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
- Polona Mihalič (@PolonaM)



Collection links
~~~~~~~~~~~~~~~~

* `Issue Tracker <https://github.com/ansible-collections/servicenow.itsm/issues>`__
* `Repository (Sources) <https://github.com/ansible-collections/servicenow.itsm>`__
