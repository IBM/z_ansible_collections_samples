#!/bin/sh

set -eu

# To reduce the amount of warnings coming from the inventory plugins
# enabled by default, we whitelist our plugin only
export ANSIBLE_INVENTORY_ENABLED=servicenow.itsm.now

# When running script-based integration targets, `ansible-test integration`
# does not make these variables available. We will need to pass them
# explicitly, as extra vars to `ansible-playbook` command.
readonly vars_file=../../integration_config.yml

# The exports below allow the inventory plugin access to authentication data
# from the environment.

eval "$(cat <<EOF | python
import yaml
with open("$vars_file") as fd:
    data = yaml.safe_load(fd)
print("export SN_HOST='{}'".format(data["sn_host"]))
print("export SN_USERNAME='{}'".format(data["sn_username"]))
print("export SN_PASSWORD='{}'".format(data["sn_password"]))
print("export SN_CLIENT_ID='{}'".format(data["sn_client_id"]))
print("export SN_CLIENT_SECRET='{}'".format(data["sn_client_secret"]))
EOF
)"

env | grep SN_

# Each inventory source `files/{name}.now.yml` represents a separate context
# for testing. The tests for each inventory source are in the
# `playbooks/{name}.yml` playbook.

set -x

for inventory_config in files/*.now.yml
do
  ansible-playbook \
    -i "$inventory_config" \
    -e "@$vars_file" \
    "playbooks/$(basename "$inventory_config" .now.yml).yml"
done
