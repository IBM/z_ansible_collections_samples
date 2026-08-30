from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
from ansible.playbook.task import Task
from ansible_collections.servicenow.itsm.plugins.action import configuration_item_batch


class TestValidate:
    @pytest.mark.parametrize(
        "name,args,required,typ",
        [
            # Required values must match the selected type.
            ("a", dict(a=3), True, int),
            ("a", dict(a=3.3), True, float),
            ("a", dict(a="b"), True, str),
            ("a", dict(a=[]), True, list),
            ("a", dict(a={}), True, dict),
            # Optional values are not checked for type-correctness if they are
            # missing.
            ("a", dict(), False, int),
            ("a", dict(), False, float),
            ("a", dict(), False, str),
            ("a", dict(), False, list),
            ("a", dict(), False, dict),
        ],
    )
    def test_valid_values(self, name, args, required, typ):
        result = configuration_item_batch.validate(name, args, required, typ)
        assert result == []

    def test_missing_required(self):
        result = configuration_item_batch.validate("a", {}, True, str)
        assert result == ["a is required argument"]

    def test_invalid_type(self):
        result = configuration_item_batch.validate("a", dict(a=3), True, str)
        assert result == ["a should be <class 'str'>"] or result == [
            "a should be <type 'str'>"
        ]


class TestValidateArguments:
    def test_valid_all_args(self):
        assert [] == configuration_item_batch.ActionModule.validate_arguments(
            dict(dataset=[{}], map={})
        )

    def test_invalid_dataset(self):
        result = configuration_item_batch.ActionModule.validate_arguments(
            dict(dataset={}, map={})
        )

        assert len(result) == 1
        assert "dataset" in result[0]

    def test_invalid_map(self):
        result = configuration_item_batch.ActionModule.validate_arguments(
            dict(dataset=[{}], map=[])
        )

        assert len(result) == 1
        assert "map" in result[0]


class TestBuildAssetArgs:
    def test_mapping_with_one_dataset(self):
        result = configuration_item_batch.ActionModule.build_asset(
            dict(
                vm_inst_id="instance_id",
                ip_address="public_ip_address",
                name="tags.Name",
            ),
            [
                dict(
                    instance_id=12345,
                    public_ip_address="1.2.3.4",
                    tags=dict(
                        Name="my_name",
                    ),
                ),
            ],
        )

        assert result == [
            dict(
                vm_inst_id="12345",
                ip_address="1.2.3.4",
                name="my_name",
            )
        ]

    def test_mapping_with_two_datasets(self):
        result = configuration_item_batch.ActionModule.build_asset(
            dict(
                vm_inst_id="instance_id",
                ip_address="public_ip_address",
                name="tags.Name",
            ),
            [
                dict(
                    instance_id=12345,
                    public_ip_address="1.2.3.4",
                    tags=dict(
                        Name="my_name",
                    ),
                ),
                dict(
                    instance_id=54321,
                    public_ip_address="4.3.2.1",
                    tags=dict(
                        Name="other_name",
                    ),
                ),
            ],
        )

        assert result == [
            dict(
                vm_inst_id="12345",
                ip_address="1.2.3.4",
                name="my_name",
            ),
            dict(
                vm_inst_id="54321",
                ip_address="4.3.2.1",
                name="other_name",
            ),
        ]

    def test_mapping_string_conversion(self):
        result = configuration_item_batch.ActionModule.build_asset(
            dict(vm_inst_id="instance_id | string"), [dict(instance_id=12345)]
        )

        assert result == [dict(vm_inst_id="12345")]

    def test_mapping_float_conversion(self):
        result = configuration_item_batch.ActionModule.build_asset(
            dict(vm_inst_id="instance_id | float"), [dict(instance_id=123.45)]
        )

        assert result == [dict(vm_inst_id="123.45")]

    def test_mapping_int_conversion(self):
        result = configuration_item_batch.ActionModule.build_asset(
            dict(vm_inst_id="instance_id | int"), [dict(instance_id=12345)]
        )

        assert result == [dict(vm_inst_id="12345")]


class TestRun:
    def test_success(self, mocker):
        task = mocker.MagicMock(
            Task,
            async_val=0,
            args=dict(
                dataset=[dict(public_ip_address="1.2.3.4")],
                map=dict(ip_address="public_ip_address"),
            ),
        )
        action = configuration_item_batch.ActionModule(
            task,
            mocker.MagicMock(),
            mocker.MagicMock(),
            loader=None,
            templar=None,
            shared_loader_obj=None,
        )
        action._execute_module = mocker.MagicMock(
            return_value=dict(ip_address="1.2.3.4")
        )

        result = action.run()

        assert result == dict(ip_address="1.2.3.4")

    def test_fail(self, mocker):
        task = mocker.MagicMock(
            Task,
            async_val=0,
            args=dict(dataset=[dict(public_ip_address="1.2.3.4")]),
        )
        action = configuration_item_batch.ActionModule(
            task,
            mocker.MagicMock(),
            mocker.MagicMock(),
            loader=None,
            templar=None,
            shared_loader_obj=None,
        )
        action._execute_module = mocker.MagicMock(
            return_value=dict(ip_address="1.2.3.4")
        )

        result = action.run()

        assert result["failed"] is True
        assert result["msg"] == "map is required argument"
