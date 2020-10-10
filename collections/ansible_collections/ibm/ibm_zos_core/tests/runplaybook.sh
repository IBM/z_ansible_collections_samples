#!/bin/bash

ansible-galaxy collection build ../ --force
ansible-galaxy collection install ibm-*.tar.gz --force

plugins_dir=$(pwd)/../plugins
export ANSIBLE_LIBRARY=${plugins_dir}/modules/
export ANSIBLE_MODULE_UTILS=${plugins_dir}/module_utils
export ANSIBLE_ACTION_PLUGINS=${plugins_dir}/action
export ANSIBLE_CONNECTION_PLUGINS=${plugins_dir}/connection
export ANSIBLE_CONFIG=$(pwd)/../playbooks/ansible.cfg

ansible-playbook -i $1 $2 -vvvv
