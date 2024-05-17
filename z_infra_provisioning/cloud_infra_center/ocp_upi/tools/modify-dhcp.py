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
import re

"""
This script will get current cluster nodes IP addresses and Mac from Ports, and then rewrite them into dhcp_template.yaml. 
So that you can use dhcp_template.yaml to configure DHCP on bastion node.
"""

def get_bastion_template():
    with open("dhcp_template.yaml", "r") as stream:
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
    cmd = "openstack --os-volume-api-version=3 port list | grep %s | awk '{print$4,$8,$6}'" % (infra_id+"-"+node_role)
    result = os.popen(cmd).read()
    """
    The example output of above command:
    $ openstack port list | grep openshift-jwblw | awk '{print$4,$8,$6}'
    openshift-jwblw-worker-port-9894 ip_address='172.26.104.222', fa:16:3e:5e:33:88
    openshift-jwblw-master-port-0 ip_address='172.26.104.119', fa:16:3e:85:39:1d
    openshift-jwblw-worker-port-2211 ip_address='172.26.104.221', fa:16:3e:dc:a2:94
    """
    nodes = result.split("\n")
    nodes_dict = {}
    for node in nodes:
        name = node.split(" ")[0]
        name = name.replace(infra_id+"-", "").replace("-port", "")
        match = re.search(r"ip_address='(.*?)'", node)
        if match:
            ip_address = match.group(1)
        else:
            break
        mac_address = node.split(',')[1].strip(" ")
        n = name.split("-")
        if len(n) <= 1:
            break
        nodes_dict[name] = {"ip": ip_address, "mac": mac_address}
    return nodes_dict

dhcp_dict = get_bastion_template()
infra_id = get_infra_id()
dhcp_dict["static_hosts"].clear()

bootstrap = get_nodes_ips(infra_id, "bootstrap")
dhcp_dict["static_hosts"].update(bootstrap)

master = get_nodes_ips(infra_id, "master")
dhcp_dict["static_hosts"].update(master)

worker = get_nodes_ips(infra_id, "worker")
dhcp_dict["static_hosts"].update(worker)

with open("dhcp_template.yaml", "w") as b:
    result = yaml.dump(dhcp_dict)
    b.write(result)
