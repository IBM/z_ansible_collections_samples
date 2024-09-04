#!/usr/bin/env python
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

import yaml
import os

"""
This script will get current cluster nodes IP addresses from Ports, and then rewrite them into cluster-template.yaml. 
So that you can use cluster-template.yaml to configure DNS and HAProxy on bastion node.
"""

def get_bastion_template():
    with open("cluster-template.yaml", "r") as stream:
        try:
            count = yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            print(exc)
    return count

def get_infra_id():
    cmd = "jq -r .infraID metadata.json"
    infra_id = os.popen(cmd).read()
    if infra_id == "":
        os._exit(1)
    infra_id = infra_id.replace("\n","")
    return infra_id

def get_nodes_ips(infra_id, node_role):
    """
     get nodes' IPs with different role, and return a dict like 
     {"masters": 
     {"master-0": {"ip": "172.26.103.1"}}, 
     {"master-1": {"ip": "172.26.103.2"}},
     {"master-2": {"ip": "172.26.103.3"}}
     }
    """
    cmd = "openstack --os-volume-api-version=3 port list | grep %s | awk '{print$4,$8}'" % (infra_id+"-"+node_role)

    result = os.popen(cmd).read()
    """
    The example output of above command:
    $ openstack port list | grep 4bzs4-worker | awk '{print$4,$8}'
    ansible-4bzs4-worker-port-1 ip_address='172.26.105.157',
    ansible-4bzs4-worker-port-0 ip_address='172.26.105.34',
    """

    nodes = result.split("\n")
    nodes_dict = {}
    for node in nodes:
        name = node.split(" ")[0]
        name = name.replace(infra_id, "")
        n = name.split("-")
        if len(n) <= 1:
            break
        elif node_role == "bootstrap":
            name = n[1]
        else:
            name = n[1] + "-" + n[-1]
        ip = node.split(" ")[1]
        ip = ip.split("'")[1]
        nodes_dict[name] = {"ip": ip}
    return nodes_dict

bastion_dict = get_bastion_template()
infra_id = get_infra_id()

bootstrap = get_nodes_ips(infra_id, "bootstrap")
bastion_dict["cluster_nodes"]["bootstrap"] = bootstrap

master = get_nodes_ips(infra_id, "master")
bastion_dict["cluster_nodes"]["masters"] = master

worker = get_nodes_ips(infra_id, "worker")
bastion_dict["cluster_nodes"]["infra"] = worker

with open("cluster-template.yaml", "w") as b:
    result = yaml.dump(bastion_dict)
    b.write(result)
