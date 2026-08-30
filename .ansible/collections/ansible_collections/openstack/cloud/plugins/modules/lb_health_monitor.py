#!/usr/bin/python

# Copyright (c) 2020 Jesper Schmitz Mouridsen.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: lb_health_monitor
author: OpenStack Ansible SIG
short_description: Add/Delete a health m nonitor to a pool in the load balancing service from OpenStack Cloud
description:
  - Add or Remove a health monitor to/from a pool in the OpenStack load-balancer service.
options:
   name:
     type: 'str'
     description:
      - Name that has to be given to the health monitor
     required: true
   state:
     type: 'str'
     description:
       - Should the resource be present or absent.
     choices: [present, absent]
     default: present
   pool:
     required: true
     type: 'str'
     description:
        - The pool name or id to monitor by the health monitor.
   type:
     type: 'str'
     default: HTTP
     description:
      - One of HTTP, HTTPS, PING, SCTP, TCP, TLS-HELLO, or UDP-CONNECT.
     choices: [HTTP, HTTPS, PING, SCTP, TCP, TLS-HELLO, UDP-CONNECT]
   delay:
     type: 'str'
     required: true
     description:
        - the interval, in seconds, between health checks.
   max_retries:
     required: true
     type: 'str'
     description:
        - The number of successful checks before changing the operating status of the member to ONLINE.
   max_retries_down:
     type: 'str'
     default: '3'
     description:
        - The number of allowed check failures before changing the operating status of the member to ERROR. A valid value is from 1 to 10. The default is 3.
   resp_timeout:
     required: true
     description:
        - The time, in seconds, after which a health check times out. Must be less than delay
     type: int
   admin_state_up:
     default: True
     description:
       - The admin state of the helath monitor true for up or false for down
     type: bool
   expected_codes:
     type: 'str'
     default: '200'
     description:
      - The list of HTTP status codes expected in response from the member to declare it healthy. Specify one of the following values
        A single value, such as 200
        A list, such as 200, 202
        A range, such as 200-204
   http_method:
     type: 'str'
     default: GET
     choices: ['GET', 'CONNECT', 'DELETE', 'HEAD', 'OPTIONS', 'PATCH', 'POST', 'PUT', 'TRACE']
     description:
     - The HTTP method that the health monitor uses for requests. One of CONNECT, DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT, or TRACE. The default is GET.
   url_path:
     type: 'str'
     default: '/'
     description:
       - The HTTP URL path of the request sent by the monitor to test the health of a backend member.
         Must be a string that begins with a forward slash (/). The default URL path is /.
requirements: ["openstacksdk"]
extends_documentation_fragment:
- openstack.cloud.openstack
'''
EXAMPLES = '''
#Create a healtmonitor named healthmonitor01 with method HEAD url_path /status and expect code 200
- openstack.cloud.lb_health_monitor:
    auth:
      auth_url: "{{keystone_url}}"
      username: "{{username}}"
      password: "{{password}}"
      project_domain_name: "{{domain_name}}"
      user_domain_name: "{{domain_name}}"
      project_name: "{{project_name}}"
    wait: true
    admin_state_up: True
    expected_codes: '200'
    max_retries_down: '4'
    http_method: GET
    url_path: "/status"
    pool: '{{pool_id}}'
    name: 'healthmonitor01'
    delay: '10'
    max_retries: '3'
    resp_timeout: '5'
    state: present
'''
RETURN = '''
health_monitor:
    description: Dictionary describing the health monitor.
    returned: On success when C(state=present)
    type: complex
    contains:
      id:
        description: The health monitor UUID.
        returned: On success when C(state=present)
        type: str
      admin_state_up:
        returned: On success when C(state=present)
        description: The administrative state of the resource.
        type: bool
      created_at:
        returned: On success when C(state=present)
        description: The UTC date and timestamp when the resource was created.
        type: str
      delay:
        returned: On success when C(state=present)
        description: The time, in seconds, between sending probes to members.
        type: int
      expected_codes:
        returned: On success when C(state=present)
        description: The list of HTTP status codes expected in response from the member to declare it healthy.
        type: str
      http_method:
        returned: On success when C(state=present)
        description: The HTTP method that the health monitor uses for requests.
        type: str
      max_retries:
        returned: On success when C(state=present)
        description: The number of successful checks before changing the operating status of the member to ONLINE.
        type: str
      max_retries_down:
        returned: On success when C(state=present)
        description: The number of allowed check failures before changing the operating status of the member to ERROR.
        type: str
      name:
        returned: On success when C(state=present)
        description: Human-readable name of the resource.
        type: str
      operating_status:
        returned: On success when C(state=present)
        description: The operating status of the resource.
        type: str
      pool_id:
        returned: On success when C(state=present)
        description: The id of the pool.
        type: str
      project_id:
        returned: On success when C(state=present)
        description: The ID of the project owning this resource.
        type: str
      provisioning_status:
        returned: On success when C(state=present)
        description: The provisioning status of the resource.
        type: str
      timeout:
        returned: On success when C(state=present)
        description: The maximum time, in seconds, that a monitor waits to connect before it times out.
        type: int
      type:
        returned: On success when C(state=present)
        description: The type of health monitor.
        type: str
      updated_at:
        returned: On success when C(state=present)
        description: The UTC date and timestamp when the resource was last updated.
        type: str
      url_path:
        returned: On success when C(state=present)
        description: The HTTP URL path of the request sent by the monitor to test the health of a backend member.
        type: str
'''
import time


from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class HealthMonitorModule(OpenStackModule):

    def _wait_for_health_monitor_status(self, health_monitor_id, status, failures, interval=5):
        timeout = self.params['timeout']

        total_sleep = 0
        if failures is None:
            failures = []

        while total_sleep < timeout:
            health_monitor = self.conn.load_balancer.get_health_monitor(health_monitor_id)
            provisioning_status = health_monitor.provisioning_status
            if provisioning_status == status:
                return health_monitor
            if provisioning_status in failures:
                self._fail_json(
                    msg="health monitor %s transitioned to failure state %s" %
                        (health_monitor, provisioning_status)
                )

        time.sleep(interval)
        total_sleep += interval

        self._fail_json(msg="timeout waiting for health monitor %s to transition to %s" %
                        (health_monitor_id, status)
                        )

    argument_spec = dict(
        name=dict(required=True),
        delay=dict(required=True),
        max_retries=dict(required=True),
        max_retries_down=dict(required=False, default="3"),
        resp_timeout=dict(required=True, type='int'),
        pool=dict(required=True),
        expected_codes=dict(required=False, default="200"),
        admin_state_up=dict(required=False, default=True, type='bool'),
        state=dict(default='present', choices=['absent', 'present']),
        http_method=dict(default="GET", required=False, choices=["GET", "CONNECT", "DELETE",
                                                                 "HEAD", "OPTIONS", "PATCH",
                                                                 "POST", "PUT", "TRACE"]),
        url_path=dict(default="/", required=False),
        type=dict(default='HTTP',
                  choices=['HTTP', 'HTTPS', 'PING', 'SCTP', 'TCP', 'TLS-HELLO', 'UDP-CONNECT']))

    module_kwargs = dict(supports_check_mode=True)

    def run(self):

        try:
            changed = False
            health_monitor = self.conn.load_balancer.find_health_monitor(name_or_id=self.params['name'])
            pool = self.conn.load_balancer.find_pool(name_or_id=self.params['pool'])
            if self.params['state'] == 'present':
                if not health_monitor:
                    changed = True
                    health_attrs = {"pool_id": pool.id,
                                    "type": self.params["type"],
                                    "delay": self.params['delay'],
                                    "max_retries": self.params['max_retries'],
                                    "max_retries_down": self.params['max_retries_down'],
                                    "timeout": self.params['resp_timeout'],
                                    "name": self.params['name'],
                                    "admin_state_up": self.params["admin_state_up"],
                                    }
                    if self.params["type"] in ["HTTP", "HTTPS"]:
                        health_attrs["expected_codes"] = self.params["expected_codes"]
                        health_attrs["http_method"] = self.params["http_method"]
                        health_attrs["url_path"] = self.params["url_path"]

                    if self.ansible.check_mode:
                        self.exit_json(changed=True)

                    health_monitor = self.conn.load_balancer.create_health_monitor(**health_attrs)
                    if not self.params['wait']:
                        self.exit_json(changed=changed, id=health_monitor.id,
                                       health_monitor=health_monitor.to_dict())
                    else:
                        health_monitor = self._wait_for_health_monitor_status(health_monitor.id, "ACTIVE", ["ERROR"])
                        self.exit_json(changed=changed, id=health_monitor.id,
                                       health_monitor=health_monitor.to_dict())
                else:
                    self.exit_json(changed=changed, id=health_monitor.id,
                                   health_monitor=health_monitor.to_dict()
                                   )
            elif self.params['state'] == 'absent':
                if health_monitor:
                    if self.ansible.check_mode:
                        self.exit_json(changed=True)
                    self.conn.load_balancer.delete_health_monitor(health_monitor)
                    changed = True

                self.exit_json(changed=changed)
        except Exception as e:
            self.fail(msg=str(e))


def main():
    module = HealthMonitorModule()
    module()


if __name__ == "__main__":
    main()
