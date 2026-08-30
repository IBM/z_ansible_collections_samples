# -*- coding: utf-8 -*-
# Copyright: 2024, Contributors to the Ansible project
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)


from __future__ import absolute_import, division, print_function

__metaclass__ = type

DOCUMENTATION = r"""
module: service_catalog

author:
  - Cosmin Tupangiu (@tupyy)

short_description: Manage ServiceNow service catalog cart

description:
  - Add items to cart.
  - Checkout cart.
  - Submit order.

version_added: 2.6.0

extends_documentation_fragment:
  - servicenow.itsm.instance

options:
  action:
    description:
      - Type of the action.
    type: str
    choices: [ checkout, submit_order, order_now ]
    default: checkout
  items:
    description:
      - List of items to be added to the cart.
    type: list
    elements: dict
    suboptions:
      sys_id:
        description:
          - The id of the item to be added.
        type: str
        required: true
      requested_for:
        description:
          - Sys_id of the user for whom to order the specified item.
        type: str
      also_request_for:
        description:
          - List user sys_ids of other users for which to order the specified item.
        type: list
        elements: str
      quantity:
        description:
          - Quantity of the item.
        type: int
        default: 1
      variables:
        description:
          - Name-value pairs of all mandatory cart item variables.
        type: dict
"""

EXAMPLES = r"""
- name: Checkout cart
  servicenow.itsm.service_catalog:
    action: checkout
    items:
      - sys_id: 9e0bd92237b1300054b6a3549dbe5dfc
        requested_for: admin
        quantity: 2

- name: Order items
  servicenow.itsm.service_catalog:
    action: order_now
    items:
      - sys_id: 9e0bd92237b1300054b6a3549dbe5dfc
        requested_for: admin
        quantity: 2
      - sys_id: 9e0bd92237b1300054b6a3549dbe5dfc
        requested_for: admin
        quantity: 1
        variables:
          var1: value
          var2: value

- name: Create new mail
  servicenow.itsm.service_catalog:
    action: order_now
    items:
      - sys_id: 186d917a6fab7980575967ddbb3ee4f2
        requested_for: abraham.lincoln
        variables:
          new_email: test@example.com
"""

RETURN = r"""
record:
  description:
    - A list of service catalog request id.
  returned: success
  type: list
  sample:
    "request_number": "REQ0010012"
    "request_id": "cf56a3fcdb3a2300e890f71fbf9619ac"
"""


from ..module_utils import arguments, client, errors
from ..module_utils.service_catalog import CartClient, Item
from ansible.module_utils.basic import AnsibleModule


def run(module, sc_client):
    action = module.params["action"]

    if action in ("checkout", "submit_order"):
        action_fn = sc_client.checkout_cart
        if action == "submit_order":
            action_fn = sc_client.submit_order

        for item_data in module.params["items"]:
            sc_client.add_to_cart(Item(item_data))

        result = action_fn()
        return (True, [result], dict(before=None, after=[result]))

    # order_now action
    # For each item in list, order it
    results = []
    for item_data in module.params["items"]:
        result = sc_client.order_now(Item(item_data))
        results.append(result)

    return (True, results, dict(before=None, after=results))


def main():
    module_args = dict(
        arguments.get_spec("instance"),
        action=dict(
            type="str",
            choices=["checkout", "submit_order", "order_now"],
            default="checkout",
        ),
        items=dict(
            type="list",
            elements="dict",
            options=dict(
                sys_id=dict(type="str", required=True),
                requested_for=dict(type="str"),
                also_request_for=dict(type="list", elements="str"),
                quantity=dict(type="int", default=1),
                variables=dict(type="dict"),
            ),
        ),
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True,
    )

    try:
        rest_client = client.Client(**module.params["instance"])
        cart_client = CartClient(rest_client)
        changed, record, diff = run(module, cart_client)
        module.exit_json(changed=changed, record=record, diff=diff)
    except errors.ServiceNowError as e:
        module.fail_json(msg=str(e))


if __name__ == "__main__":
    main()
