.. Created with antsibull-docs 2.14.0

servicenow.itsm.now inventory -- Inventory source for ServiceNow table records.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

This inventory plugin is part of the `servicenow.itsm collection <https://galaxy.ansible.com/ui/repo/published/servicenow/itsm/>`_ (version 2.7.0).

It is not included in ``ansible-core``.
To check whether it is installed, run ``ansible-galaxy collection list``.

To install it, use: :code:`ansible-galaxy collection install servicenow.itsm`.

To use it in a playbook, specify: ``servicenow.itsm.now``.

New in servicenow.itsm 1.0.0

.. contents::
   :local:
   :depth: 1


Synopsis
--------

- Builds inventory from ServiceNow table records.
- Requires a configuration file ending in :literal:`now.yml` or :literal:`now.yaml`.
- The plugin sets host variables denoted by :emphasis:`columns`.
- For variables with dots (for example 'location.country') use lookup('ansible.builtin.vars', 'variable.name') notation. See the example section for more details. This feature is added in version 2.1.0.








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
      <div class="ansibleOptionAnchor" id="parameter-aggregation"></div>
      <p style="display: inline;"><strong>aggregation</strong></p>
      <a class="ansibleOptionLink" href="#parameter-aggregation" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.7.0</i></p>

    </td>
    <td valign="top">
      <p>Enable multiple variable values aggregations.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code style="color: blue;"><b>false</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>true</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-cache"></div>
      <p style="display: inline;"><strong>cache</strong></p>
      <a class="ansibleOptionLink" href="#parameter-cache" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>

    </td>
    <td valign="top">
      <p>Toggle to enable/disable the caching of the inventory&#x27;s source data, requires a cache plugin setup to work.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code style="color: blue;"><b>false</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>true</code></p></li>
      </ul>

      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>INI entry</p>
        <pre>[inventory]
  cache = false</pre>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_INVENTORY_CACHE</code></p>

      </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-cache_connection"></div>
      <p style="display: inline;"><strong>cache_connection</strong></p>
      <a class="ansibleOptionLink" href="#parameter-cache_connection" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>Cache connection data or path, read cache plugin documentation for specifics.</p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>INI entries</p>
        <pre>[defaults]
  fact_caching_connection = VALUE</pre>

        <pre>[inventory]
  cache_connection = VALUE</pre>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_CACHE_PLUGIN_CONNECTION</code></p>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_INVENTORY_CACHE_CONNECTION</code></p>

      </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-cache_plugin"></div>
      <p style="display: inline;"><strong>cache_plugin</strong></p>
      <a class="ansibleOptionLink" href="#parameter-cache_plugin" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>Cache plugin to use for the inventory&#x27;s source data.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;memory&#34;</code></p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>INI entries</p>
        <pre>[defaults]
  fact_caching = memory</pre>

        <pre>[inventory]
  cache_plugin = memory</pre>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_CACHE_PLUGIN</code></p>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_INVENTORY_CACHE_PLUGIN</code></p>

      </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-cache_prefix"></div>
      <p style="display: inline;"><strong>cache_prefix</strong></p>
      <a class="ansibleOptionLink" href="#parameter-cache_prefix" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>Prefix to use for cache plugin files/tables</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;ansible_inventory_&#34;</code></p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>INI entries</p>
        <pre>[defaults]
  fact_caching_prefix = ansible_inventory_</pre>

        <pre>[inventory]
  cache_prefix = ansible_inventory_</pre>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_CACHE_PLUGIN_PREFIX</code></p>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_INVENTORY_CACHE_PLUGIN_PREFIX</code></p>

      </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-cache_timeout"></div>
      <p style="display: inline;"><strong>cache_timeout</strong></p>
      <a class="ansibleOptionLink" href="#parameter-cache_timeout" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">integer</span>
      </p>

    </td>
    <td valign="top">
      <p>Cache duration in seconds</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">3600</code></p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>INI entries</p>
        <pre>[defaults]
  fact_caching_timeout = 3600</pre>

        <pre>[inventory]
  cache_timeout = 3600</pre>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_CACHE_PLUGIN_TIMEOUT</code></p>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_INVENTORY_CACHE_TIMEOUT</code></p>

      </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-columns"></div>
      <p style="display: inline;"><strong>columns</strong></p>
      <a class="ansibleOptionLink" href="#parameter-columns" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=string</span>
      </p>

    </td>
    <td valign="top">
      <p>List of <em>table</em> columns to be included as hostvars.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">[&#34;name&#34;, &#34;host_name&#34;, &#34;fqdn&#34;, &#34;ip_address&#34;]</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-compose"></div>
      <p style="display: inline;"><strong>compose</strong></p>
      <a class="ansibleOptionLink" href="#parameter-compose" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>

    </td>
    <td valign="top">
      <p>Create vars from jinja2 expressions.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">{}</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-enhanced"></div>
      <p style="display: inline;"><strong>enhanced</strong></p>
      <a class="ansibleOptionLink" href="#parameter-enhanced" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.3.0</i></p>

    </td>
    <td valign="top">
      <p>Enable enhanced inventory which provides relationship information from CMDB.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code style="color: blue;"><b>false</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>true</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-groups"></div>
      <p style="display: inline;"><strong>groups</strong></p>
      <a class="ansibleOptionLink" href="#parameter-groups" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">dictionary</span>
      </p>

    </td>
    <td valign="top">
      <p>Add hosts to group based on Jinja2 conditionals.</p>
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
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">{}</code></p>
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
      <p>If provided, it requires <em>client_secret</em>.</p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_CLIENT_ID</code></p>

      </li>
      </ul>
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
      <p>If provided, it requires <em>client_id</em>.</p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_CLIENT_SECRET</code></p>

      </li>
      </ul>
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
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.4.0</i></p>

    </td>
    <td valign="top">
      <p>Grant type used for OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_GRANT_TYPE</code> environment variable will be used.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code style="color: blue;"><b>&#34;password&#34;</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>&#34;refresh_token&#34;</code></p></li>
      </ul>

      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_GRANT_TYPE</code></p>

      </li>
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
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_HOST</code></p>

      </li>
      </ul>
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
        / <span style="color: red;">required</span>
      </p>

    </td>
    <td valign="top">
      <p>Password used for authentication.</p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_PASSWORD</code></p>

      </li>
      </ul>
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
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 1.4.0</i></p>

    </td>
    <td valign="top">
      <p>Refresh token used for OAuth authentication.</p>
      <p>If not set, the value of the <code class='docutils literal notranslate'>SN_REFRESH_TOKEN</code> environment variable will be used.</p>
      <p>Required when <em>grant_type=refresh_token</em>.</p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_REFRESH_TOKEN</code></p>

      </li>
      </ul>
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
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_TIMEOUT</code></p>

      </li>
      </ul>
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
        / <span style="color: red;">required</span>
      </p>

    </td>
    <td valign="top">
      <p>Username used for authentication.</p>
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_USERNAME</code></p>

      </li>
      </ul>
    </td>
  </tr>

  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-inventory_hostname_source"></div>
      <p style="display: inline;"><strong>inventory_hostname_source</strong></p>
      <a class="ansibleOptionLink" href="#parameter-inventory_hostname_source" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>The column to use for inventory hostnames.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;name&#34;</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups"></div>
      <p style="display: inline;"><strong>keyed_groups</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">list</span>
        / <span style="color: purple;">elements=dictionary</span>
      </p>

    </td>
    <td valign="top">
      <p>Add hosts to group based on the values of a variable.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">[]</code></p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups/default_value"></div>
      <p style="display: inline;"><strong>default_value</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups/default_value" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in ansible-core 2.12</i></p>

    </td>
    <td valign="top">
      <p>The default value when the host variable&#x27;s value is an empty string.</p>
      <p>This option is mutually exclusive with <code class="ansible-option literal notranslate"><strong><a class="reference internal" href="#parameter-keyed_groups/trailing_separator"><span class="std std-ref"><span class="pre">keyed_groups[].trailing_separator</span></span></a></strong></code>.</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups/key"></div>
      <p style="display: inline;"><strong>key</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups/key" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>The key from input dictionary used to generate groups</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups/parent_group"></div>
      <p style="display: inline;"><strong>parent_group</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups/parent_group" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>parent group for keyed group</p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups/prefix"></div>
      <p style="display: inline;"><strong>prefix</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups/prefix" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>A keyed group name will start with this prefix</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;&#34;</code></p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups/separator"></div>
      <p style="display: inline;"><strong>separator</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups/separator" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>separator used to build the keyed group name</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;_&#34;</code></p>
    </td>
  </tr>
  <tr>
    <td></td>
    <td valign="top">
      <div class="ansibleOptionAnchor" id="parameter-keyed_groups/trailing_separator"></div>
      <p style="display: inline;"><strong>trailing_separator</strong></p>
      <a class="ansibleOptionLink" href="#parameter-keyed_groups/trailing_separator" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in ansible-core 2.12</i></p>

    </td>
    <td valign="top">
      <p>Set this option to <code class="ansible-value literal notranslate">False</code> to omit the <code class="ansible-option literal notranslate"><strong><a class="reference internal" href="#parameter-keyed_groups/separator"><span class="std std-ref"><span class="pre">keyed_groups[].separator</span></span></a></strong></code> after the host variable when the value is an empty string.</p>
      <p>This option is mutually exclusive with <code class="ansible-option literal notranslate"><strong><a class="reference internal" href="#parameter-keyed_groups/default_value"><span class="std std-ref"><span class="pre">keyed_groups[].default_value</span></span></a></strong></code>.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>false</code></p></li>
        <li><p><code style="color: blue;"><b>true</b></code> <span style="color: blue;">← (default)</span></p></li>
      </ul>

    </td>
  </tr>

  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-leading_separator"></div>
      <p style="display: inline;"><strong>leading_separator</strong></p>
      <a class="ansibleOptionLink" href="#parameter-leading_separator" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in ansible-core 2.11</i></p>

    </td>
    <td valign="top">
      <p>Use in conjunction with keyed_groups.</p>
      <p>By default, a keyed group that does not have a prefix or a separator provided will have a name that starts with an underscore.</p>
      <p>This is because the default prefix is "" and the default separator is "_".</p>
      <p>Set this option to False to omit the leading underscore (or other separator) if no prefix is given.</p>
      <p>If the group name is derived from a mapping the separator is still used to concatenate the items.</p>
      <p>To not use a separator in the group name at all, set the separator for the keyed group to an empty string instead.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>false</code></p></li>
        <li><p><code style="color: blue;"><b>true</b></code> <span style="color: blue;">← (default)</span></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-plugin"></div>
      <p style="display: inline;"><strong>plugin</strong></p>
      <a class="ansibleOptionLink" href="#parameter-plugin" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
        / <span style="color: red;">required</span>
      </p>

    </td>
    <td valign="top">
      <p>The name of the ServiceNow Inventory Plugin.</p>
      <p>This should always be <code class='docutils literal notranslate'>servicenow.itsm.now</code>.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code>&#34;servicenow.itsm.now&#34;</code></p></li>
      </ul>

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
      <div class="ansibleOptionAnchor" id="parameter-strict"></div>
      <p style="display: inline;"><strong>strict</strong></p>
      <a class="ansibleOptionLink" href="#parameter-strict" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>

    </td>
    <td valign="top">
      <p>If <code class="ansible-value literal notranslate">yes</code> make invalid entries a fatal error, otherwise skip and continue.</p>
      <p>Since it is possible to use facts in the expressions they might not always be available and we ignore those errors by default.</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code style="color: blue;"><b>false</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>true</code></p></li>
      </ul>

    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-sysparm_limit"></div>
      <p style="display: inline;"><strong>sysparm_limit</strong></p>
      <a class="ansibleOptionLink" href="#parameter-sysparm_limit" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">integer</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in servicenow.itsm 2.5.0</i></p>

    </td>
    <td valign="top">
      <p>Control the maximum number of records returned in a single query.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">1000</code></p>
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
      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>Environment variable: <code>SN_SYSPARM_QUERY</code></p>

      </li>
      </ul>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-table"></div>
      <p style="display: inline;"><strong>table</strong></p>
      <a class="ansibleOptionLink" href="#parameter-table" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">string</span>
      </p>

    </td>
    <td valign="top">
      <p>The ServiceNow table to use as the inventory source.</p>
      <p style="margin-top: 8px;"><b style="color: blue;">Default:</b> <code style="color: blue;">&#34;cmdb_ci_server&#34;</code></p>
    </td>
  </tr>
  <tr>
    <td colspan="2" valign="top">
      <div class="ansibleOptionAnchor" id="parameter-use_extra_vars"></div>
      <p style="display: inline;"><strong>use_extra_vars</strong></p>
      <a class="ansibleOptionLink" href="#parameter-use_extra_vars" title="Permalink to this option"></a>
      <p style="font-size: small; margin-bottom: 0;">
        <span style="color: purple;">boolean</span>
      </p>
      <p><i style="font-size: small; color: darkgreen;">added in ansible-core 2.11</i></p>

    </td>
    <td valign="top">
      <p>Merge extra vars into the available variables for composition (highest precedence).</p>
      <p style="margin-top: 8px;"><b">Choices:</b></p>
      <ul>
        <li><p><code style="color: blue;"><b>false</b></code> <span style="color: blue;">← (default)</span></p></li>
        <li><p><code>true</code></p></li>
      </ul>

      <p style="margin-top: 8px;"><b>Configuration:</b></p>
      <ul>
      <li>
        <p>INI entry</p>
        <pre>[inventory_plugins]
  use_extra_vars = false</pre>

      </li>
      <li>
        <p>Environment variable: <code>ANSIBLE_INVENTORY_USE_EXTRA_VARS</code></p>

      </li>
      </ul>
    </td>
  </tr>
  </tbody>
  </table>




Notes
-----

- Query feature and constructed groups were added in version 1.2.0.
- Caching feature added in version 2.5.0.


Examples
--------

.. code-block:: yaml

    # A trivial example that creates a host from every record of the
    # ServiceNow cmdb_ci_server table. The ip_address column is used for
    # for ansible host, and server name for inventory hostname.
    # No groups will be created - all the resulting hosts are ungrouped.
    plugin: servicenow.itsm.now

    # `ansible-inventory -i inventory.now.yaml --graph` output:
    # @all:
    #  |--@ungrouped:
    #  |  |--DatabaseServer1
    #  |  |--DatabaseServer2
    #  |  |--INSIGHT-NY-03
    #  |  |--MailServerUS
    #  |  |--VMWARE-SD-04


    # Group hosts automatically, according to values of the manufacturer column.
    plugin: servicenow.itsm.now
    keyed_groups:
      - key: manufacturer
        separator: ""

    # `ansible-inventory -i inventory.now.yaml --graph` output:
    # @all:
    #  |--@Dell Inc.:
    #  |  |--DatabaseServer1
    #  |  |--DatabaseServer2
    #  |  |--INSIGHT-NY-03
    #  |--@Lenovo:
    #  |  |--FileServerFloor1
    #  |  |--FileServerFloor2
    #  |--@ungrouped:

    # Group hosts automatically, according to values of the os column. Filtering ensures
    # that we only see selected operating systems.
    plugin: servicenow.itsm.now
    query:
      - os: = Linux Red Hat
      - os: = Windows XP
    keyed_groups:
      - key: os
        prefix: os

    # `ansible-inventory -i inventory.now.yaml --graph` output:
    #  |--@os_Linux_Red_Hat:
    #  |  |--DatabaseServer1
    #  |  |--DatabaseServer2
    #  |--@os_Windows_XP:
    #  |  |--FileServerFloor1
    #  |  |--FileServerFloor2
    #  |  |--INSIGHT-NY-03
    #  |--@ungrouped:

    # Group hosts into named according to the specified criteria. Here, we created a group
    # of non-Windows production servers.
    plugin: servicenow.itsm.now
    groups:
      non_windows_prod_servers: >-
        classification == "Production" and
        os not in ("Windows XP", "Windows 2000", "Windows 2000 Server")

    # `ansible-inventory -i inventory.now.yaml --graph` output:
    # @all:
    #  |--@non_windows_prod_servers:
    #  |  |--DatabaseServer2
    #  |  |--PS LinuxApp01
    #  |  |--PS LinuxApp02
    #  |  |--lnux100
    #  |  |--lnux101

    # Add composed variables to hosts. In the following example, we created a cost variable
    # that contains an amount and a currency, and set the ansible_host variable to the fqdn
    # listed in the record.
    plugin: servicenow.itsm.now
    inventory_hostname_source: asset_tag
    columns:
      - name
      - classification
      - cpu_type
    compose:
        cost: cost ~ " " ~ cost_cc
        ansible_host: fqdn

    # `ansible-inventory -i inventory.now.yaml --graph --vars` output:
    # @all:
    #  |--@ungrouped:
    #  |  |--P1000019
    #  |  |  |--{ansible_host = my.server.com}
    #  |  |  |--{classification = Production}
    #  |  |  |--{cost = 100 USD}
    #  |  |  |--{cpu_type = Intel}
    #  |  |  |--{name = SAP-SD-02}

    # Similar to the example above, but use enhanced groups with relationship information instead.
    plugin: servicenow.itsm.now
    enhanced: true
    strict: true
    inventory_hostname_source: asset_tag
    columns:
      - name
      - classification
      - cpu_type
      - cost
    compose:
        cost: cost ~ " " ~ cost_cc
        ansible_host: fqdn

    # `ansible-inventory -i inventory.now.yaml --graph --vars` output:
    # @all:
    # |--@Blackberry_Depends_on:
    # |  |--P1000201
    # |  |  |--{ansible_host = my.server.com}
    # |  |  |--{classification = Production}
    # |  |  |--{cost = 2,160 USD}
    # |  |  |--{cpu_type = Intel}
    # |  |  |--{name = INSIGHT-NY-03}

    plugin: servicenow.itsm.now
    enhanced: false
    strict: true
    table: cmdb_ci_server
    columns:
      - name
      - ip_address
      - location
      - location.country
    compose:
      street: location
      country: lookup('ansible.builtin.vars', 'location.country')

    # `ansible-inventory -i inventory.now.yaml --graph --vars` output:
    # @all:
    # |--@ungrouped:
    # |  |--OWA-SD-01
    # |  |  |--{country = Italy}
    # |  |  |--{ip_address = }
    # |  |  |--{location = Via Nomentana 56, Rome}
    # |  |  |--{location.country = Italy}
    # |  |  |--{name = OWA-SD-01}
    # |  |  |--{street = Via Nomentana 56, Rome}

    # Use a javascript function defined in ServiceNow under "Script Includes",
    # which returns a list of the sys_ids that match a certain criteria
    # Example of script:
    # function MyFunction(key_entry) {
    #   var cis = [];
    #   var key_value = new GlideRecord("cmdb_key_value");
    #   key_value.addEncodedQuery("keyLIKE"+key_entry);
    #   key_value.query();
    #   while (key_value.next()) {
    #     cis.push(key_value.configuration_item + '');
    #   }
    #   return cis;
    # }
    # Other examples in https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html
    plugin: servicenow.itsm.now
    table: cmdb_ci_server
    query:
      - sys_id: 'IN javascript:MyFunction("xyz")'
    keyed_groups:
      - key: os
        prefix: os

    # `ansible-inventory -i inventory.now.yaml --graph` output:
    # @all:
    # |--@ungrouped:
    # |--@os_linux:
    # |  |--node2
    # |  |--node3
    # |  |--node1






Authors
~~~~~~~

- Manca Bizjak (@mancabizjak)
- Miha Dolinar (@mdolin)
- Tadej Borovsak (@tadeboro)
- Uros Pascinski (@uscinski)


.. hint::
    Configuration entries for each entry type have a low to high priority order. For example, a variable that is lower in the list will override a variable that is higher up.

Collection links
~~~~~~~~~~~~~~~~

* `Issue Tracker <https://github.com/ansible-collections/servicenow.itsm/issues>`__
* `Repository (Sources) <https://github.com/ansible-collections/servicenow.itsm>`__
