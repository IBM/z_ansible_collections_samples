from __future__ import absolute_import, division, print_function

__metaclass__ = type

import sys

import pytest
from ansible_collections.servicenow.itsm.plugins.modules import configuration_item_batch

pytestmark = pytest.mark.skipif(
    sys.version_info < (2, 7), reason="requires python2.7 or higher"
)


class TestUpdate:
    def test_update_create_record(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_class_name="cmdb_ci_ec2_instance",
                id_column_set=["vm_inst_id"],
                dataset=[
                    dict(vm_inst_id="12345", ip_address="1.2.3.4", name="my_name")
                ],
            )
        )
        table_client.get_record.return_value = None

        result, changed = configuration_item_batch.update(module, table_client)

        table_client.create_record.assert_called_once()
        table_client.update_record.assert_not_called()
        assert changed is True

    def test_update_is_superset(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_class_name="cmdb_ci_ec2_instance",
                id_column_set=["vm_inst_id"],
                dataset=[
                    dict(vm_inst_id="12345", ip_address="1.2.3.4", name="my_name")
                ],
            )
        )

        table_client.get_record.return_value = dict(
            ip_address="1.2.3.4", name="my_name", vm_inst_id="12345"
        )

        result, changed = configuration_item_batch.update(module, table_client)

        table_client.create_record.assert_not_called()
        table_client.update_record.assert_not_called()
        assert changed is False

    def test_update_update_record(self, create_module, table_client):
        module = create_module(
            params=dict(
                instance=dict(
                    host="https://my.host.name", username="user", password="pass"
                ),
                sys_class_name="cmdb_ci_ec2_instance",
                id_column_set=["vm_inst_id"],
                dataset=[
                    dict(vm_inst_id="12345", ip_address="1.2.3.4", name="my_name")
                ],
            )
        )

        table_client.get_record.return_value = dict(
            ip_address="1.1.1.1", name="my_name", vm_inst_id="12345"
        )

        result, changed = configuration_item_batch.update(module, table_client)

        table_client.create_record.assert_not_called()
        table_client.update_record.assert_called_once()
        assert changed is True
