from __future__ import absolute_import, division, print_function

__metaclass__ = type

import pytest
from ansible.parsing.dataloader import DataLoader
from ansible.playbook.task import Task
from ansible.template import Templar
from ansible_collections.servicenow.itsm.plugins.action import api


class TestRun:
    def test_success(self, mocker):
        task = mocker.MagicMock(
            Task,
            async_val=0,
            args=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                sys_id="my_sys_id",
                action="patch",
                data=dict(state="new"),
            ),
        )
        action = api.ActionModule(
            task,
            mocker.MagicMock(),
            mocker.MagicMock(),
            loader=None,
            templar=None,
            shared_loader_obj=None,
        )
        action._execute_module = mocker.MagicMock(
            return_value=dict(
                number="INC0000001",
                short_description="Test incident",
                impact="3",
                urgency="3",
                sys_id="1234",
                state="old",
            )
        )

        result = action.run()

        assert result == dict(
            number="INC0000001",
            short_description="Test incident",
            impact="3",
            urgency="3",
            sys_id="1234",
            state="old",
        )

    def test_fail(self, mocker):
        # Test mutual exclusiveness of data and template is tested on controller machine already
        with pytest.raises(
            TypeError, match="parameters are mutually exclusive: data|template"
        ):
            task = mocker.MagicMock(
                Task,
                async_val=0,
                args=dict(
                    instance=dict(
                        host="my.host.name", username="user", password="pass"
                    ),
                    resource="incident",
                    sys_id="my_sys_id",
                    action="patch",
                    data=dict(state="new"),
                    template="/path/to/the/template.j2",
                ),
            )
            action = api.ActionModule(
                task,
                mocker.MagicMock(),
                mocker.MagicMock(),
                loader=None,
                templar=None,
                shared_loader_obj=None,
            )

            action._execute_module = mocker.MagicMock(
                return_value=dict(
                    number="INC0000001",
                    short_description="Test incident",
                    impact="3",
                    urgency="3",
                    sys_id="1234",
                    state="old",
                )
            )

            action.run()


class TestTemplateArgs:
    def test_template_args(self):
        template = "/path/to/the/template.j2"
        assert api.get_template_args(template) == dict(
            newline_sequence="\n",
            variable_start_string=None,
            variable_end_string=None,
            block_start_string=None,
            block_end_string=None,
            trim_blocks=True,
            lstrip_blocks=False,
            path="/path/to/the/template.j2",
        )


class TestDefaultEnv:
    def test_set_default_env(self, mocker):
        task = mocker.MagicMock(
            Task,
            async_val=0,
            args=dict(
                instance=dict(host="my.host.name", username="user", password="pass"),
                resource="incident",
                sys_id="my_sys_id",
                action="patch",
                data=dict(state="new"),
            ),
        )
        action = api.ActionModule(
            task,
            mocker.MagicMock(),
            mocker.MagicMock(),
            loader=None,
            templar=Templar(loader=DataLoader()),
            shared_loader_obj=None,
        )

        result = action._set_default_env()

        assert result == dict(
            newline_sequence="\n",
            variable_start_string="{{",
            variable_end_string="}}",
            block_start_string="{%",
            block_end_string="%}",
            trim_blocks=True,
        )
