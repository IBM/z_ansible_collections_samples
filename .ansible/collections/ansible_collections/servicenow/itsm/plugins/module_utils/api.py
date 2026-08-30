# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

SN_QUERY_MAPPING = dict(
    query="sysparm_query",
    display_value="sysparm_display_value",
    exclude_reference_link="sysparm_exclude_reference_link",
    suppress_pagination_header="sysparm_suppress_pagination_header",
    fields="sysparm_fields",
    columns="sysparm_fields",
    query_category="sysparm_query_category",
    limit="sysparm_limit",
    view="sysparm_view",
    query_no_domain="sysparm_query_no_domain",
    no_count="sysparm_no_count",
    sys_id="sys_id",
    sysparm_query="sysparm_query",
)

# FIELD_COLUMNS_NAME and FIELD_QUERY_NAME are the ones that are additionally modified in api_info.py,
# so setting constant variable for them.
FIELD_COLUMNS_NAME = "columns"
FIELD_QUERY_NAME = "sysparm_query"
FIELD_SYS_ID = "sys_id"
FIELD_DATA = "data"
FIELD_TEMPLATE = "template"
FIELD_DATA_RENDERED = "data_rendered"
FIELD_QUERY_PARAMS = "query_params"
FIELD_ATTRIBUTES_NAME = "attributes"

POSSIBLE_FILTER_PARAMETERS = [
    FIELD_QUERY_NAME,
    "display_value",
    "exclude_reference_link",
    "fields",
    "query_category",
    "query_no_domain",
    "no_count",
    FIELD_COLUMNS_NAME,
    FIELD_SYS_ID,
]

ACTION_POST = "post"
ACTION_PATCH = "patch"
ACTION_DELETE = "delete"


def transform_query_to_servicenow_query(query):
    """
    Transforms query by applying SN_QUERY_MAPPING to query's keys.
    """
    return dict(
        (SN_QUERY_MAPPING[query_key], query_value)
        for (query_key, query_value) in query.items()
    )


def table_name(module):
    """
    In api.py and api_info.py the table's name is always going to be stored in module's resource
    Deprecated in v.2.5.0
    """
    return module.params["resource"]


def resource_name(module):
    """
    Return either the api_path or the table name from modules' parameters
    """
    return module.params["resource"] or module.params["api_path"]


def get_query_by_sys_id(module):
    """Deprecated in v2.4.0."""
    return dict(sys_id=module.params[FIELD_SYS_ID])


def get_sys_id(module):
    """Return the sys_id from module's parameters"""
    return module.params[FIELD_SYS_ID]


def field_present(module, field):
    return field in module.params and module.params[field]
