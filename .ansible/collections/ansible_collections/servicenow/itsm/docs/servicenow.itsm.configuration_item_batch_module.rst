.. Created with antsibull-docs 2.14.0

servicenow.itsm.configuration_item_batch module -- Manage ServiceNow configuration items in batch mode
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This module is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.configuration_item_batch``.

New in servicenow.itsm 1.2.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Create, update ServiceNow configuration items in batch mode.
- For more information, refer to the ServiceNow configuration management documentation at \ `https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-table-property-descriptions.html <https://docs.servicenow.com/bundle/tokyo-servicenow-platform/page/product/configuration-management/reference/cmdb-table-property-descriptions.html>`__.

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
      <div class="ansibleOptionAnchor" id="parameter-dataset"></div>
      <p style="display: inline;"><strong>dataset</strong></p>
      <a class="ansibleOptionLink" href="#parameter-dataset" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=dictionary</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>List of dictionaries that will be used as a data source.</p>
      <p>Each item in a list represents one CMDB item.</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-id_column_set"></div>
      <p style="display: inline;"><strong>id_column_set</strong></p>
      <a class="ansibleOptionLink" href="#parameter-id_column_set" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=string</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>Columns that should be used to identify an existing record that we need to update.</p>
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
      <div class="ansibleOptionAnchor" id="parameter-map"></div>
      <p style="display: inline;"><strong>map</strong></p>
      <a class="ansibleOptionLink" href="#parameter-map" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>Transformation instructions on how to convert input data to CMDB items.</p>
      <p>Keys represent the CMDB item column names and the values are Jinja expressions that extract the value from the source data.</p>
      <p>Data is returned as string because ServiceNow API expect this</p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-sys_class_name"></div>
      <p style="display: inline;"><strong>sys_class_name</strong></p>
      <a class="ansibleOptionLink" href="#parameter-sys_class_name" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
        / <span style="color: red;">required</span>
      </p>
    </td>
    <td valign="top">
      <p>Table name (configuration item type) that we would like to manipulate.</p>
    </td>
  </tr>
  </tbody>
  </table>





See Also
--------

* `servicenow.itsm.configuration\_item <configuration_item_module.rst>`__

  Manage ServiceNow configuration items.
* `servicenow.itsm.configuration\_item\_info <configuration_item_info_module.rst>`__

  List ServiceNow configuration item.

Examples
--------

.. code-block:: yaml

    - name: Update CMDB with some data
      servicenow.itsm.configuration_item_batch:
        sys_class_name: cmdb_ci_ec2_instance
        id_column_set: vm_inst_id
        dataset:
          - instance_id: 12345
            public_ip_address: 1.2.3.4
            tags:
              Name: my_name
          - instance_id: 54321
            public_ip_address: 4.3.2.1
            tags:
              Name: other_name
        map:
          vm_inst_id: instance_id
          ip_address: public_ip_address
          name: tags.Name

    - name: Identify CMDB item using combination of two columns
      servicenow.itsm.configuration_item_batch:
        sys_class_name: cmdb_ci_server
        id_column_set:
          - name
          - ip_address
        dataset: "{{ input_data }}"
        map:
          name: tags.Name
          ip_address: private_ip_address




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
      <p>A list of configuration item records.</p>
      <p>Note that the fields of the returned records depend on the configuration item&#x27;s <em>sys_class_name</em>.</p>
      <p>Returning of values added in version 2.0.0.</p>
      <p style="margin-top: 8px;"><b>Returned:</b> success</p>
      <p style="margin-top: 8px; color: blue; word-wrap: break-word; word-break: break-all;"><b style="color: black;">Sample:</b> <code>[{&#34;asset&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/alm_asset/04a96c0d3790200044e0bfc8bcbe5db3&#34;, &#34;value&#34;: &#34;04a96c0d3790200044e0bfc8bcbe5db3&#34;}, &#34;asset_tag&#34;: &#34;P1000503&#34;, &#34;assigned&#34;: &#34;2019-11-10 07:00:00&#34;, &#34;assigned_to&#34;: {&#34;link&#34;: &#34;https://www.example.comapi/now/table/sys_user/92826bf03710200044e0bfc8bcbe5dbb&#34;, &#34;value&#34;: &#34;92826bf03710200044e0bfc8bcbe5dbb&#34;}, &#34;assignment_group&#34;: &#34;&#34;, &#34;checked_in&#34;: &#34;&#34;, &#34;checked_out&#34;: &#34;&#34;, &#34;company&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/core_company/81fbfe03ac1d55eb286d832de58ae1fd&#34;, &#34;value&#34;: &#34;81fbfe03ac1d55eb286d832de58ae1fd&#34;}, &#34;cost&#34;: &#34;1799.99&#34;, &#34;cost_cc&#34;: &#34;USD&#34;, &#34;cost_center&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/cmn_cost_center/d9d0a971c0a80a641c20b13d99a48576&#34;, &#34;value&#34;: &#34;d9d0a971c0a80a641c20b13d99a48576&#34;}, &#34;delivery_date&#34;: &#34;2019-06-09 08:00:00&#34;, &#34;department&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/cmn_department/221f79b7c6112284005d646b76ab978c&#34;, &#34;value&#34;: &#34;221f79b7c6112284005d646b76ab978c&#34;}, &#34;due&#34;: &#34;&#34;, &#34;due_in&#34;: &#34;&#34;, &#34;gl_account&#34;: &#34;&#34;, &#34;install_date&#34;: &#34;2019-07-28 07:00:00&#34;, &#34;install_status&#34;: &#34;1&#34;, &#34;invoice_number&#34;: &#34;&#34;, &#34;justification&#34;: &#34;&#34;, &#34;lease_id&#34;: &#34;&#34;, &#34;location&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/cmn_location/8226baa4ac1d55eb40eb653c02649519&#34;, &#34;value&#34;: &#34;8226baa4ac1d55eb40eb653c02649519&#34;}, &#34;managed_by&#34;: &#34;&#34;, &#34;manufacturer&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/core_company/b7e9e843c0a80169009a5a485bb2a2b5&#34;, &#34;value&#34;: &#34;b7e9e843c0a80169009a5a485bb2a2b5&#34;}, &#34;model_id&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/cmdb_model/d501454f1b1310002502fbcd2c071334&#34;, &#34;value&#34;: &#34;d501454f1b1310002502fbcd2c071334&#34;}, &#34;name&#34;: &#34;MacBook Pro 15\&#34;&#34;, &#34;order_date&#34;: &#34;2019-05-13 08:00:00&#34;, &#34;owned_by&#34;: &#34;&#34;, &#34;po_number&#34;: &#34;PO100003&#34;, &#34;purchase_date&#34;: &#34;2019-05-25&#34;, &#34;serial_number&#34;: &#34;ABE-486-V17263-DO&#34;, &#34;skip_sync&#34;: &#34;false&#34;, &#34;support_group&#34;: &#34;&#34;, &#34;supported_by&#34;: &#34;&#34;, &#34;sys_class_name&#34;: &#34;cmdb_ci_computer&#34;, &#34;sys_class_path&#34;: &#34;/!!/!2/!(&#34;, &#34;sys_created_by&#34;: &#34;admin&#34;, &#34;sys_created_on&#34;: &#34;2012-02-18 08:14:21&#34;, &#34;sys_domain&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/sys_user_group/global&#34;, &#34;value&#34;: &#34;global&#34;}, &#34;sys_domain_path&#34;: &#34;/&#34;, &#34;sys_id&#34;: &#34;00a96c0d3790200044e0bfc8bcbe5db4&#34;, &#34;sys_mod_count&#34;: &#34;6&#34;, &#34;sys_tags&#34;: &#34;&#34;, &#34;sys_updated_by&#34;: &#34;system&#34;, &#34;sys_updated_on&#34;: &#34;2022-03-18 03:59:41&#34;, &#34;unverified&#34;: &#34;false&#34;, &#34;vendor&#34;: {&#34;link&#34;: &#34;https://www.example.com/api/now/table/core_company/b7e9e843c0a80169009a5a485bb2a2b5&#34;, &#34;value&#34;: &#34;b7e9e843c0a80169009a5a485bb2a2b5&#34;}, &#34;warranty_expiration&#34;: &#34;2022-07-27&#34;}]</code></p>
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
