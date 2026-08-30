# User Mapping

Users can customize any `Choice list` within the ServiceNow application. One of them is `incident state`, which has the following default values.

| Value | Label       | Ansible label |
|-------|-------------|---------------|
|       |             | absent        |
|     1 | New         | new           |
|     2 | In progress | in_progress   |
|     3 | On hold     | on_hold       |
|     6 | Resolved    | resolved      |
|     7 | Closed      | closed        |
|     8 | Canceled    | canceled      | 

For example, we also want to have the `Testing` state inside the ServiceNow app, so we simply add it under `System Definition -> Choice Lists`.

| Value | Label       | Ansible label |
|-------|-------------|---------------|
|   ... | ...         | ...           |
|     9 | Testing     | /             |

When we do that, the `servicenow.itsm` collection does not have mapping for the `incident state` named `Testing`. 

## Solution

The precondition for the solution was that all labels within the `servicenow.itsm` collection must be stable. This means that renaming the label within ServiceNow must not
break any playbook that uses `serviceNow.itsm`.

To achieve that, we added a user mapping argument within each module. For example, within `servicenow.itsm.incident_info`:

```yaml
- name: Retrieve all incidents
  servicenow.itsm.incident_info:
    incident_mapping:
      state:
        1: "new"
        2: "in_progress"
        3: "on_hold"
        6: "resolved"
        7: "closed"
        8: "canceled"
        9: "test"
  register: result
```

The end result is that when `servicenow.itsm.incident_info` module is used, state `Testing` is also known. The matching is done by key, which has value `9` in the example.

## Modules with mapping and fields that can be overridden
All modules within `servicenow.itsm` support partial overriding of arguments. In the example above, only the `state` list was overridden, all others were left intact.
Here is the full list of arguments that can be overridden.


### Incident

Modules:
- `servicenow.itsm.incident`
- `servicenow.itsm.incident_info`

Argument: `incident_mapping`

Overridable items: `impact`, `urgency`, `state`, `hold_reason`

### Change request

Modules:
 - `servicenow.itsm.change_request`
 - `servicenow.itsm.change_request_info`

Argument: `change_request_mapping`

Overridable items: `priority`, `risk`, `impact`, `urgency`, `state`

### Change request task

Modules:
- `servicenow.itsm.change_request_task`
- `servicenow.itsm.change_request_task_info`

Argument: `change_request_task_mapping`

Overridable items: `state`

### Configuration item

Modules:
- `servicenow.itsm.configuration_item`
- `servicenow.itsm.configutation_item_info`

Argument: `configuration_item_mapping`

Overridable items: `environment`, `install_status`, `operational_status`


### Problem

Modules:
- `servicenow.itsm.problem`
- `servicenow.itsm.problem_info`

Argument: `problem_mapping`

Overridable items: `impact`, `urgency`, `problem_state`, `state`

### Problem task

Modules:
- `servicenow.itsm.problem_task`
- `servicenow.itsm.problem_task_info`

Argument: `problem_task_mapping`

Overridable items: `state`, `priority`


## Complete example
In this example, we show how the user can define all user mappings in one place. And then use them in any task within the playbook.


### Custom user mapping at `Incident` module
```yaml
- environment:
    SN_HOST: "{{ sn_host }}"
    SN_USERNAME: "{{ sn_username }}"
    SN_PASSWORD: "{{ sn_password }}"

  vars:
    mapping:
      incident:
        impact:
          1: "high"
          2: "medium"
          3: "low"
        urgency:
          1: "high"
          2: "medium"
          3: "low"
        state:
          1: "new"
          2: "in_progress"
          3: "on_hold"
          6: "resolved"
          7: "closed"
          8: "canceled"
          9: "test"
        hold_reason:
          "": ""
          1: "awaiting_caller"
          3: "awaiting_problem"
          4: "awaiting_vendor"
          5: "awaiting_change"


  block:
    - name: Retrieve all incidents
      servicenow.itsm.incident_info:
        incident_mapping: "{{ mapping.incident }}"
      register: result

    - ansible.builtin.assert:
        that:
          - result.records != []

    - name: Create test incident - check mode
      servicenow.itsm.incident: &create-incident
        incident_mapping: "{{ mapping.incident }}"
        caller: admin
        state: new
        short_description: Test incident
        impact: low
        urgency: low
        attachments:
          - path: targets/incident_with_mapping/res/sample_file.txt
      register: result
    ```
