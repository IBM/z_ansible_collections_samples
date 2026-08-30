# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
name: now
author:
  - Manca Bizjak (@mancabizjak)
  - Miha Dolinar (@mdolin)
  - Tadej Borovsak (@tadeboro)
  - Uros Pascinski (@uscinski)
short_description: Inventory source for ServiceNow table records.
description:
  - Builds inventory from ServiceNow table records.
  - Requires a configuration file ending in C(now.yml) or C(now.yaml).
  - The plugin sets host variables denoted by I(columns).
  - For variables with dots (for example 'location.country') use lookup('ansible.builtin.vars', 'variable.name') notation.
    See the example section for more details. This feature is added in version 2.1.0.
version_added: 1.0.0
extends_documentation_fragment:
  - ansible.builtin.constructed
  - inventory_cache
notes:
  - Query feature and constructed groups were added in version 1.2.0.
  - Caching feature added in version 2.5.0.
options:
  plugin:
    description:
      - The name of the ServiceNow Inventory Plugin.
      - This should always be C(servicenow.itsm.now).
    required: true
    type: str
    choices: [ servicenow.itsm.now ]
  instance:
    description:
      - ServiceNow instance information.
    type: dict
    default: {}
    suboptions:
      host:
        description:
          - The ServiceNow host name.
        env:
          - name: SN_HOST
        required: true
        type: str
      username:
        description:
          - Username used for authentication.
        env:
          - name: SN_USERNAME
        required: true
        type: str
      password:
        description:
          - Password used for authentication.
        env:
          - name: SN_PASSWORD
        required: true
        type: str
      client_id:
        description:
          - ID of the client application used for OAuth authentication.
          - If provided, it requires I(client_secret).
        env:
          - name: SN_CLIENT_ID
        type: str
      client_secret:
        description:
          - Secret associated with I(client_id). Used for OAuth authentication.
          - If provided, it requires I(client_id).
        env:
          - name: SN_CLIENT_SECRET
        type: str
      grant_type:
        description:
          - Grant type used for OAuth authentication.
          - If not set, the value of the C(SN_GRANT_TYPE) environment variable will be used.
        choices: [ 'password', 'refresh_token' ]
        default: password
        env:
          - name: SN_GRANT_TYPE
        type: str
        version_added: 1.4.0
      refresh_token:
        description:
          - Refresh token used for OAuth authentication.
          - If not set, the value of the C(SN_REFRESH_TOKEN) environment
            variable will be used.
          - Required when I(grant_type=refresh_token).
        env:
          - name: SN_REFRESH_TOKEN
        type: str
        version_added: 1.4.0
      timeout:
        description:
          - Timeout in seconds for the connection with the ServiceNow instance.
        env:
          - name: SN_TIMEOUT
        type: float
  table:
    description: The ServiceNow table to use as the inventory source.
    type: str
    default: cmdb_ci_server
  columns:
    description:
      - List of I(table) columns to be included as hostvars.
    type: list
    elements: str
    default: [name, host_name, fqdn, ip_address]
  enhanced:
    description:
      - Enable enhanced inventory which provides relationship information from CMDB.
    type: bool
    default: false
    version_added: 1.3.0
  aggregation:
    description:
      - Enable multiple variable values aggregations.
    type: bool
    default: false
    version_added: 2.7.0
  inventory_hostname_source:
    type: str
    description:
      - The column to use for inventory hostnames.
    default: name
  query:
    description:
      - Provides a set of operators for use with filters, condition builders, and encoded queries.
      - The data type of a field determines what operators are available for it.
        Refer to the ServiceNow Available Filters Queries documentation at
        U(https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html).
      - Mutually exclusive with C(sysparm_query).
    type: list
    elements: dict
  sysparm_query:
    description:
      - An encoded query string used to filter the results as an alternative to C(query).
      - Refer to the ServiceNow Available Filters Queries documentation at
        U(https://docs.servicenow.com/bundle/tokyo-platform-user-interface/page/use/common-ui-elements/reference/r_OpAvailableFiltersQueries.html).
      - If not set, the value of the C(SN_SYSPARM_QUERY) environment, if specified.
      - Mutually exclusive with C(query).
    type: str
    env:
      - name: SN_SYSPARM_QUERY
    version_added: 2.0.0
  sysparm_limit:
    description:
      - Control the maximum number of records returned in a single query.
    type: int
    default: 1000
    version_added: 2.5.0

"""

EXAMPLES = r"""
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
"""


import os
import hashlib

from ansible.errors import AnsibleParserError
from ansible.inventory.group import to_safe_group_name as orig_safe
from ansible.plugins.inventory import (
    BaseInventoryPlugin,
    Constructable,
    Cacheable,
    to_safe_group_name,
)
from ansible.utils.vars import combine_vars

from ..module_utils.client import Client
from ..module_utils.errors import ServiceNowError
from ..module_utils.query import parse_query, serialize_query
from ..module_utils.relations import (
    REL_FIELDS,
    REL_QUERY,
    REL_TABLE,
    enhance_records_with_rel_groups,
)
from ..module_utils.table import TableClient


class Aggregator:
    def __init__(self, columns):
        self.data = dict()
        self.tmp = None

    def add(self, host, key, value):
        parent, child = self._split(key)
        if not self.tmp:
            self.tmp = dict()

        if not child:
            self.tmp[parent] = value
        else:
            tmp_parent_data = self.tmp.get(parent)
            if not tmp_parent_data:
                tmp_parent_data = dict()

            parent_data = dict()
            parent_data[child] = value
            if isinstance(tmp_parent_data, str):
                parent_data[parent] = tmp_parent_data
            else:
                for k, v in tmp_parent_data.items():
                    parent_data[k] = v

            self.tmp[parent] = parent_data

    def commit(self, host):
        if not self.tmp:
            return
        host_data = self.data.get(host, dict())
        for k, v in self.tmp.items():
            if k in host_data:
                vv = host_data.get(k)
                if isinstance(vv, list):
                    if not self._is_exists(vv, v):
                        vv.append(v)
                continue
            if isinstance(v, dict):
                host_data[k] = [v]
                continue
            host_data[k] = v
        self.tmp = None
        self.data[host] = host_data

    def aggregate(self, inventory):
        for host, data in self.data.items():
            for k, v in data.items():
                inventory.set_variable(host, k, v)

    def _split(self, column):
        if "." not in column:
            return column, ""
        parts = column.split(".")
        return parts[0], parts[1]

    def _is_exists(self, items, item):
        hash_v = self._hash_dict(item)
        for i in items:
            if self._hash_dict(i) == hash_v:
                return True
        return False

    def _hash_dict(self, d):
        h = hashlib.sha256()
        d_sorted = str(dict(sorted(d.items())))
        h.update(d_sorted.encode())
        return h.hexdigest()


def construct_sysparm_query(query, is_encoded_query):
    if is_encoded_query:
        return query
    parsed, err = parse_query(query)
    if err:
        raise AnsibleParserError(err)
    return serialize_query(parsed)


def fetch_records(table_client, table, query, fields=None, is_encoded_query=False):
    snow_query = dict(
        # Make references and choice fields human-readable
        sysparm_display_value=True,
    )
    if query:
        snow_query["sysparm_query"] = construct_sysparm_query(query, is_encoded_query)
    if fields:
        snow_query["sysparm_fields"] = ",".join(fields)

    return table_client.list_records(table, snow_query)


class ConstructableWithLookup(Constructable):
    def _compose(self, template, variables):
        """helper method for plugins to compose variables for Ansible based on jinja2 expression and inventory vars"""
        t = self.templar

        try:
            use_extra = self.get_option("use_extra_vars")
        except Exception:
            use_extra = False

        if use_extra:
            t.available_variables = combine_vars(variables, self._vars)
        else:
            t.available_variables = variables

        """ Only change that we have overridden is that we do not disable lookups"""
        return t.template(
            "%s%s%s"
            % (
                t.environment.variable_start_string,
                template,
                t.environment.variable_end_string,
            ),
            disable_lookups=False,
        )


class InventoryModule(BaseInventoryPlugin, ConstructableWithLookup, Cacheable):
    NAME = "servicenow.itsm.now"

    # Constructable methods use the _sanitize_group_name class method to filter out
    # invalid characters from group names. By default, characters that are not valid in
    # python variables, are always replaced by underscores. We are overriding this with
    # a function that respects the TRANSFORM_INVALID_GROUP_CHARS configuration option
    # and allows users to control the behavior.
    _sanitize_group_name = staticmethod(orig_safe)

    def verify_file(self, path):
        if super(InventoryModule, self).verify_file(path):
            if path.endswith(("now.yaml", "now.yml")):
                return True
            self.display.vvv(
                'Skipping due to inventory source not ending in "now.yaml" nor "now.yml"'
            )
        return False

    def add_host(self, record, name_source):
        if name_source not in record:
            msg = "Inventory hostname source column '{0}' is not present in the record."
            raise AnsibleParserError(msg.format(name_source))

        inventory_hostname = record[name_source]
        if inventory_hostname:
            host = self.inventory.add_host(inventory_hostname)
            return host

        self.display.warning(
            "Skipping host {0} due to empty {1}".format(record["sys_id"], name_source)
        )
        return None

    def set_hostvars(self, host, record, columns):
        missing = set(columns).difference(record)
        if missing:
            raise AnsibleParserError(
                "Invalid column names: {0}.".format(", ".join(missing))
            )

        for k in columns:
            self.inventory.set_variable(host, k.replace(".", "_"), record[k])

    def set_host_vars_aggregated(self, host, record, columns, aggregator):
        for k in columns:
            aggregator.add(host, k, record[k])
        aggregator.commit(host)

    def fill_constructed(
        self,
        records,
        columns,
        name_source,
        compose,
        groups,
        keyed_groups,
        strict,
        enhanced,
        aggregation,
    ):
        if aggregation:
            aggregator = Aggregator(columns)

        for record in records:
            host = self.add_host(record, name_source)
            if host:
                if aggregation:
                    self.set_host_vars_aggregated(host, record, columns, aggregator)
                else:
                    self.set_hostvars(host, record, columns)

                self._set_composite_vars(compose, record, host, strict)
                self._add_host_to_composed_groups(groups, record, host, strict)
                self._add_host_to_keyed_groups(keyed_groups, record, host, strict)
                if enhanced:
                    self.fill_enhanced_auto_groups(record, host)
        if aggregation:
            aggregator.aggregate(self.inventory)

    def fill_enhanced_auto_groups(self, record, host):
        for rel_group in record["relationship_groups"]:
            rel_group = to_safe_group_name(rel_group)
            self.inventory.add_group(rel_group)
            self.inventory.add_child(rel_group, host)

    def _merge_instance_config(self, instance_config, instance_env):
        # Pulls the values from the environment, and if necessary, overrides
        # with configuration provided in the inventory source.
        instance = instance_env.copy()
        given_keys = instance_config.keys()
        to_override = set(instance.keys()).intersection(given_keys)
        for option in to_override:
            instance[option] = instance_config[option]
        return instance

    def _get_instance_from_env(self):
        def get_secret_from_env():
            for arg in ("SN_CLIENT_SECRET", "SN_SECRET_ID"):
                value = os.getenv(arg)
                if value is not None:
                    if arg == "SN_SECRET_ID":
                        # Remove this in 3.0.0
                        self.display.deprecated(
                            "Setting environment variable 'SN_SECRET_ID' is being removed "
                            "in favor of 'SN_CLIENT_SECRET'",
                            version="3.0.0",
                            collection_name="servicenow.itsm",
                        )
                    return value
            return None

        def get_timeout_from_env(default=120):
            try:
                return float(os.getenv("SN_TIMEOUT"))
            except (ValueError, TypeError):
                return default

        return dict(
            host=os.getenv("SN_HOST"),
            username=os.getenv("SN_USERNAME"),
            password=os.getenv("SN_PASSWORD"),
            client_id=os.getenv("SN_CLIENT_ID"),
            client_secret=get_secret_from_env(),
            refresh_token=os.getenv("SN_REFRESH_TOKEN"),
            grant_type=os.getenv("SN_GRANT_TYPE"),
            timeout=get_timeout_from_env(),
        )

    def _get_instance(self):
        instance_config = self.get_option("instance")
        instance_env = self._get_instance_from_env()
        return self._merge_instance_config(instance_config, instance_env)

    def _construct_cache_suffix(self):
        """
        Return the cache suffix constructued from either query or sysparm_query.
        As the query can be a list of dict elements, key and values are encoded in base64.
        The result is base64 encoded.
        """

        def __encode(s):
            from base64 import b64encode

            return b64encode(s.encode()).decode()

        suffix = ""
        if self.get_option("query"):
            for query in self.get_option("query"):
                for k, v in query.items():
                    if suffix:
                        suffix = "{0}_{1}_{2}".format(suffix, k, v)
                    else:
                        suffix = "{0}_{1}".format(k, v)
        elif self.get_option("sysparm_query"):
            suffix = self.get_option("sysparm_query")
        else:
            return ""
        return __encode(suffix)

    def parse(self, inventory, loader, path, cache=True):
        super(InventoryModule, self).parse(inventory, loader, path)

        self._read_config_data(path)
        self.cache_key = self.get_cache_key(path)
        cache_sub_key = "/".join(
            [
                self._get_instance()["host"].rstrip("/"),
                "table",
                self.get_option("table"),
                self._construct_cache_suffix(),
            ]
        )

        self.use_cache = self.get_option("cache") and cache
        self.update_cache = self.get_option("cache") and not cache

        try:
            client = Client(**self._get_instance())
        except ServiceNowError as e:
            raise AnsibleParserError(e)

        enhanced = self.get_option("enhanced")
        aggregation = self.get_option("aggregation")

        sysparm_limit = self.get_option("sysparm_limit")
        if sysparm_limit:
            table_client = TableClient(client, batch_size=sysparm_limit)
        else:
            table_client = TableClient(client)

        table = self.get_option("table")
        name_source = self.get_option("inventory_hostname_source")
        columns = self.get_option("columns")

        query = self.get_option("query")
        sysparm_query = self.get_option("sysparm_query")

        if query and sysparm_query:
            raise AnsibleParserError(
                "Invalid configuration: 'query' and 'sysparm_query' are mutually "
                "exclusive."
            )

        records = []

        if not self.update_cache:
            try:
                records = self._cache[self.cache_key][cache_sub_key]
            except KeyError:
                pass

        if not records:
            if self.cache_key not in self._cache:
                self._cache[self.cache_key] = {path: ""}

            records = fetch_records(
                table_client,
                table,
                query or sysparm_query,
                is_encoded_query=bool(sysparm_query),
            )

            referenced_columns = [x for x in columns if "." in x]
            if referenced_columns:
                referenced_records = fetch_records(
                    table_client,
                    table,
                    query or sysparm_query,
                    fields=referenced_columns + ["sys_id"],
                    is_encoded_query=bool(sysparm_query),
                )

                referenced_dict = dict((x["sys_id"], x) for x in referenced_records)
                # Keep track of processed 'sys_id' to avoid popping it twice if there were duplicates returned by ServiceNow.
                processed_records = []
                for record in records:
                    referenced = referenced_dict.get(record["sys_id"], None)
                    if referenced:
                        if record["sys_id"] not in processed_records:
                            referenced.pop("sys_id")
                        processed_records.append(record["sys_id"])
                        for key, value in referenced.items():
                            record[key] = value

            if enhanced:
                rel_records = fetch_records(
                    table_client, REL_TABLE, REL_QUERY, fields=REL_FIELDS
                )
                enhance_records_with_rel_groups(records, rel_records)

            self._cache[self.cache_key] = {cache_sub_key: records}

        self.fill_constructed(
            records,
            columns,
            name_source,
            self.get_option("compose"),
            self.get_option("groups"),
            self.get_option("keyed_groups"),
            self.get_option("strict"),
            enhanced,
            aggregation,
        )
