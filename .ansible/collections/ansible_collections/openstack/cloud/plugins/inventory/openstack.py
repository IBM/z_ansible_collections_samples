# -*- coding: utf-8 -*-
# Copyright (c) 2012, Marco Vito Moscaritolo <marco@agavee.com>
# Copyright (c) 2013, Jesse Keating <jesse.keating@rackspace.com>
# Copyright (c) 2015, Hewlett-Packard Development Company, L.P.
# Copyright (c) 2016, Rackspace Australia
# Copyright (c) 2017 Ansible Project
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


DOCUMENTATION = '''
---
name: openstack
author: OpenStack Ansible SIG
short_description: OpenStack inventory source
description:
    - Get inventory hosts from OpenStack clouds
    - Uses openstack.(yml|yaml) YAML configuration file to configure the inventory plugin
    - Uses standard clouds.yaml YAML configuration file to configure cloud credentials
options:
    plugin:
        description: token that ensures this is a source file for the 'openstack' plugin.
        required: True
        choices: ['openstack', 'openstack.cloud.openstack']
    show_all:
        description: toggles showing all vms vs only those with a working IP
        type: bool
        default: false
    inventory_hostname:
        description: |
            What to register as the inventory hostname.
            If set to 'uuid' the uuid of the server will be used and a
            group will be created for the server name.
            If set to 'name' the name of the server will be used unless
            there are more than one server with the same name in which
            case the 'uuid' logic will be used.
            Default is to do 'name', which is the opposite of the old
            openstack.py inventory script's option use_hostnames)
        type: string
        choices:
            - name
            - uuid
        default: "name"
    use_names:
        description: |
            Use the host's 'name' instead of 'interface_ip' for the 'ansible_host' and
            'ansible_ssh_host' facts. This might be desired when using jump or
            bastion hosts and the name is the FQDN of the host.
        type: bool
        default: false
    expand_hostvars:
        description: |
            Run extra commands on each host to fill in additional
            information about the host. May interrogate cinder and
            neutron and can be expensive for people with many hosts.
            (Note, the default value of this is opposite from the default
            old openstack.py inventory script's option expand_hostvars)
        type: bool
        default: false
    private:
        description: |
            Use the private interface of each server, if it has one, as
            the host's IP in the inventory. This can be useful if you are
            running ansible inside a server in the cloud and would rather
            communicate to your servers over the private network.
        type: bool
        default: false
    only_clouds:
        description: |
            List of clouds from clouds.yaml to use, instead of using
            the whole list.
        type: list
        elements: str
        default: []
    fail_on_errors:
        description: |
            Causes the inventory to fail and return no hosts if one cloud
            has failed (for example, bad credentials or being offline).
            When set to False, the inventory will return as many hosts as
            it can from as many clouds as it can contact. (Note, the
            default value of this is opposite from the old openstack.py
            inventory script's option fail_on_errors)
        type: bool
        default: false
    all_projects:
        description: |
            Lists servers from all projects
        type: bool
        default: false
    clouds_yaml_path:
        description: |
            Override path to clouds.yaml file. If this value is given it
            will be searched first. The default path for the
            ansible inventory adds /etc/ansible/openstack.yaml and
            /etc/ansible/openstack.yml to the regular locations documented
            at https://docs.openstack.org/os-client-config/latest/user/configuration.html#config-files
        type: list
        elements: str
        env:
            - name: OS_CLIENT_CONFIG_FILE
    compose:
        description: Create vars from jinja2 expressions.
        type: dictionary
        default: {}
    groups:
        description: Add hosts to group based on Jinja2 conditionals.
        type: dictionary
        default: {}
    legacy_groups:
        description: Automatically create groups from host variables.
        type: bool
        default: true
requirements:
    - "python >= 3.6"
    - "openstacksdk >= 0.28, < 0.99.0"
extends_documentation_fragment:
- inventory_cache
- constructed

'''

EXAMPLES = '''
# file must be named openstack.yaml or openstack.yml
# Make the plugin behave like the default behavior of the old script
plugin: openstack.cloud.openstack
expand_hostvars: yes
fail_on_errors: yes
all_projects: yes
'''

import collections
import sys
import logging

from ansible.errors import AnsibleParserError
from ansible.plugins.inventory import BaseInventoryPlugin, Constructable, Cacheable
from ansible.utils.display import Display
from ansible_collections.openstack.cloud.plugins.module_utils.openstack import (
    ensure_compatibility
)

display = Display()
os_logger = logging.getLogger("openstack")

try:
    # Due to the name shadowing we should import other way
    import importlib
    sdk = importlib.import_module('openstack')
    sdk_inventory = importlib.import_module('openstack.cloud.inventory')
    client_config = importlib.import_module('openstack.config.loader')
    sdk_exceptions = importlib.import_module("openstack.exceptions")
    HAS_SDK = True
except ImportError:
    display.vvvv("Couldn't import Openstack SDK modules")
    HAS_SDK = False


class InventoryModule(BaseInventoryPlugin, Constructable, Cacheable):
    ''' Host inventory provider for ansible using OpenStack clouds. '''

    NAME = 'openstack.cloud.openstack'

    def parse(self, inventory, loader, path, cache=True):

        super(InventoryModule, self).parse(inventory, loader, path)

        cache_key = self._get_cache_prefix(path)

        # file is config file
        self._config_data = self._read_config_data(path)

        msg = ''
        if not self._config_data:
            msg = 'File empty. this is not my config file'
        elif 'plugin' in self._config_data and self._config_data['plugin'] not in (self.NAME, 'openstack'):
            msg = 'plugin config file, but not for us: %s' % self._config_data['plugin']
        elif 'plugin' not in self._config_data and 'clouds' not in self._config_data:
            msg = "it's not a plugin configuration nor a clouds.yaml file"
        elif not HAS_SDK:
            msg = "openstacksdk is required for the OpenStack inventory plugin. OpenStack inventory sources will be skipped."

        if not msg:
            try:
                ensure_compatibility(sdk.version.__version__)
            except ImportError as e:
                msg = ("Incompatible openstacksdk library found: {error}."
                       .format(error=str(e)))

        if msg:
            display.vvvv(msg)
            raise AnsibleParserError(msg)

        if 'clouds' in self._config_data:
            self.display.vvvv(
                "Found clouds config file instead of plugin config. "
                "Using default configuration."
            )
            self._config_data = {}

        # update cache if the user has caching enabled and the cache is being refreshed
        # will update variable below in the case of an expired cache
        cache_needs_update = not cache and self.get_option('cache')

        if cache:
            cache = self.get_option('cache')
        source_data = None
        if cache:
            self.display.vvvv("Reading inventory data from cache: %s" % cache_key)
            try:
                source_data = self._cache[cache_key]
            except KeyError:
                # cache expired or doesn't exist yet
                display.vvvv("Inventory data cache not found")
                cache_needs_update = True

        if not source_data:
            self.display.vvvv("Getting hosts from Openstack clouds")
            clouds_yaml_path = self._config_data.get('clouds_yaml_path')
            if clouds_yaml_path:
                config_files = (
                    clouds_yaml_path
                    + client_config.CONFIG_FILES
                )
            else:
                config_files = None

            # Redict logging to stderr so it does not mix with output
            # particular ansible-inventory JSON output
            # TODO(mordred) Integrate openstack's logging with ansible's logging
            if self.display.verbosity > 3:
                sdk.enable_logging(debug=True, stream=sys.stderr)
            else:
                sdk.enable_logging(stream=sys.stderr)

            cloud_inventory = sdk_inventory.OpenStackInventory(
                config_files=config_files,
                private=self._config_data.get('private', False))
            self.display.vvvv("Found %d cloud(s) in Openstack" %
                              len(cloud_inventory.clouds))
            only_clouds = self._config_data.get('only_clouds', [])
            if only_clouds and not isinstance(only_clouds, list):
                raise ValueError(
                    'OpenStack Inventory Config Error: only_clouds must be'
                    ' a list')
            if only_clouds:
                new_clouds = []
                for cloud in cloud_inventory.clouds:
                    self.display.vvvv("Looking at cloud : %s" % cloud.name)
                    if cloud.name in only_clouds:
                        self.display.vvvv("Selecting cloud : %s" % cloud.name)
                        new_clouds.append(cloud)
                cloud_inventory.clouds = new_clouds

            self.display.vvvv("Selected %d cloud(s)" %
                              len(cloud_inventory.clouds))

            expand_hostvars = self._config_data.get('expand_hostvars', False)
            fail_on_errors = self._config_data.get('fail_on_errors', False)
            all_projects = self._config_data.get('all_projects', False)
            self.use_names = self._config_data.get('use_names', False)

            source_data = []
            try:
                source_data = cloud_inventory.list_hosts(
                    expand=expand_hostvars, fail_on_cloud_config=fail_on_errors,
                    all_projects=all_projects)
            except Exception as e:
                self.display.warning("Couldn't list Openstack hosts. "
                                     "See logs for details")
                os_logger.error(e.message)
            finally:
                if cache_needs_update:
                    self._cache[cache_key] = source_data

        self._populate_from_source(source_data)

    def _populate_from_source(self, source_data):
        groups = collections.defaultdict(list)
        firstpass = collections.defaultdict(list)
        hostvars = {}

        use_server_id = (
            self._config_data.get('inventory_hostname', 'name') != 'name')
        show_all = self._config_data.get('show_all', False)

        for server in source_data:
            if 'interface_ip' not in server and not show_all:
                continue
            firstpass[server['name']].append(server)

        for name, servers in firstpass.items():
            if len(servers) == 1 and not use_server_id:
                self._append_hostvars(hostvars, groups, name, servers[0])
            else:
                server_ids = set()
                # Trap for duplicate results
                for server in servers:
                    server_ids.add(server['id'])
                if len(server_ids) == 1 and not use_server_id:
                    self._append_hostvars(hostvars, groups, name, servers[0])
                else:
                    for server in servers:
                        self._append_hostvars(
                            hostvars, groups, server['id'], server,
                            namegroup=True)

        self._set_variables(hostvars, groups)

    def _set_variables(self, hostvars, groups):

        strict = self.get_option('strict')

        # set vars in inventory from hostvars
        for host in hostvars:

            # actually update inventory
            for key in hostvars[host]:
                self.inventory.set_variable(host, key, hostvars[host][key])

            # create composite vars
            self._set_composite_vars(
                self._config_data.get('compose'), self.inventory.get_host(host).get_vars(), host, strict)

            # constructed groups based on conditionals
            self._add_host_to_composed_groups(
                self._config_data.get('groups'), hostvars[host], host, strict)

            # constructed groups based on jinja expressions
            self._add_host_to_keyed_groups(
                self._config_data.get('keyed_groups'), hostvars[host], host, strict)

        for group_name, group_hosts in groups.items():
            gname = self.inventory.add_group(group_name)
            for host in group_hosts:
                if gname == host:
                    display.vvvv("Same name for host %s and group %s" % (host, gname))
                    self.inventory.add_host(host, gname)
                else:
                    self.inventory.add_child(gname, host)

    def _get_groups_from_server(self, server_vars, namegroup=True):
        groups = []

        region = server_vars['region']
        cloud = server_vars['cloud']
        metadata = server_vars.get('metadata', {})

        # Create a group for the cloud
        groups.append(cloud)

        # Create a group on region
        if region:
            groups.append(region)

        # And one by cloud_region
        groups.append("%s_%s" % (cloud, region))

        # Check if group metadata key in servers' metadata
        if 'group' in metadata:
            groups.append(metadata['group'])

        for extra_group in metadata.get('groups', '').split(','):
            if extra_group:
                groups.append(extra_group.strip())

        groups.append('instance-%s' % server_vars['id'])
        if namegroup:
            groups.append(server_vars['name'])

        for key in ('flavor', 'image'):
            if 'name' in server_vars[key]:
                groups.append('%s-%s' % (key, server_vars[key]['name']))

        for key, value in iter(metadata.items()):
            groups.append('meta-%s_%s' % (key, value))

        az = server_vars.get('az', None)
        if az:
            # Make groups for az, region_az and cloud_region_az
            groups.append(az)
            groups.append('%s_%s' % (region, az))
            groups.append('%s_%s_%s' % (cloud, region, az))
        return groups

    def _append_hostvars(self, hostvars, groups, current_host,
                         server, namegroup=False):
        if not self.use_names:
            hostvars[current_host] = dict(
                ansible_ssh_host=server['interface_ip'],
                ansible_host=server['interface_ip'],
                openstack=server,
            )

        if self.use_names:
            hostvars[current_host] = dict(
                ansible_ssh_host=server['name'],
                ansible_host=server['name'],
                openstack=server,
            )

        self.inventory.add_host(current_host)

        if self.get_option('legacy_groups'):
            for group in self._get_groups_from_server(server, namegroup=namegroup):
                groups[group].append(current_host)

    def verify_file(self, path):

        if super(InventoryModule, self).verify_file(path):
            for fn in ('openstack', 'clouds'):
                for suffix in ('yaml', 'yml'):
                    maybe = '{fn}.{suffix}'.format(fn=fn, suffix=suffix)
                    if path.endswith(maybe):
                        self.display.vvvv("Valid plugin config file found")
                        return True
        return False
