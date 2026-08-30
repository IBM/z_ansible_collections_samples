#!/usr/bin/python
# -*- coding: utf-8 -*-
# Copyright: (c) 2022, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


DOCUMENTATION = r"""
module: attachment_info

author:
  - Polona Mihalič (@PolonaM)

short_description: a module that users can use to download attachment using sys_id

description:
  - Download attachment using attachment's sys_id.
version_added: 2.0.0
extends_documentation_fragment:
  - servicenow.itsm.instance

options:
  dest:
    description:
      - Specifies the path in which the attachment will be downloaded to.
      - The file will be downloaded to all of the hosts from the inventory.
      - All the directories on the path should already exist.
      - If the file at the destination path already exists, it will be overwritten.
    type: path
    required: true
  sys_id:
    description:
      - Attachment's sys_id.
    type: str
    required: true

notes:
  - Supports check_mode.
"""

EXAMPLES = """
  - name: ServiceNow download attachment module
    servicenow.itsm.attachment:
      instance:
        host: https://instance_id.service-now.com
        username: user
        password: pass
      dest: /tmp/sn-attachment
      sys_id: 003a3ef24ff1120031577d2ca310c74b
"""


RETURN = r"""
record:
  description: download attachment record
  returned: success
  type: dict
  contains:
    elapsed:
        description: the number of seconds that elapsed while performing the download
        returned: success
        type: float
        sample: 2.3
    msg:
        description: OK or error message
        returned: always
        type: str
        sample: OK
    size:
        description: size of the attachment in bytes
        returned: success
        type: int
        sample: 1220
    status_code:
        description: the HTTP status code from the request
        returned: success
        type: int
        sample: 200
"""


import json
import os
import time

from ansible.module_utils.basic import AnsibleModule

from ..module_utils import arguments, attachment, client, errors


def run(module, attachment_client):
    start = time.time()
    response = attachment_client.get_attachment(module.params["sys_id"])
    if response.status == 200:
        if not module.check_mode:
            attachment_client.save_attachment(response.data, module.params["dest"])
    elif response.status == 404:
        fallback_msg = "Not found"
        fallback_dict = dict(detail=fallback_msg)
        error = response.json.get("error", fallback_dict).get("detail", fallback_msg)
        raise errors.ServiceNowError("Status code: 404, Details: " + error)
    end = time.time()
    elapsed = round(end - start, 2)
    try:
        size = int(json.loads(response.headers["x-attachment-metadata"])["size_bytes"])
    except KeyError:
        size = os.path.getsize(module.params["dest"])
    status_code = response.status
    msg = "OK"

    return {
        "size": size,
        "elapsed": elapsed,
        "status_code": status_code,
        "msg": msg,
        # add sys_id
    }


def main():
    module_args = dict(
        arguments.get_spec("instance"),
        # Overwrites sys_id from SHARED_SPECS to add required=True
        sys_id=dict(
            type="str",
            required=True,
        ),
        dest=dict(
            type="path",
            required=True,
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
    )

    try:
        snow_client = client.Client(**module.params["instance"])
        attachment_client = attachment.AttachmentClient(snow_client)
        record = run(module, attachment_client)
        module.exit_json(changed=True, record=record)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
