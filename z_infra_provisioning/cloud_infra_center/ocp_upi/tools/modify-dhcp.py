#!/usr/bin/env python
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

import os
import re
"""
This script will get current cluster nodes IP addresses and Mac from Ports, and then rewrite them into dhcp_template.yaml. 
So that you can use dhcp_template.yaml to configure DHCP on bastion node.

Here is the default template:
static_hosts:
  bootstrap:
    mac: "00:11:22:33:44:55"
    ip: "10.1.0.10"
  master-0:
    mac: "00:11:22:33:44:56"
    ip: "10.1.0.11"
  master-1:
    mac: "00:11:22:33:44:57"
    ip: "10.1.0.12"
  master-2:
    mac: "00:11:22:33:44:58"
    ip: "10.1.0.13"
  worker-0:
    mac: "00:11:22:33:44:59"
    ip: "10.1.0.14"
  worker-1:
    mac: "00:11:22:33:44:60"
    ip: "10.1.0.15"
  worker-2:
    mac: "00:11:22:33:44:61"
    ip: "10.1.0.16"
"""
template = "static_hosts:"
cmd = "jq -r .infraID metadata.json"
infra_id = os.popen(cmd).read()
if infra_id == "":
    os._exit(1)
infra_id = infra_id.replace("\n","")
cmd = "openstack --os-volume-api-version=3 port list | grep %s | awk '{print$4,$8,$6}'" % (infra_id)
result = os.popen(cmd).read()
"""
The example output of above command:
$ openstack port list | grep openshift-jwblw | awk '{print$4,$8,$6}'
openshift-jwblw-worker-port-9894 ip_address='172.26.104.222', fa:16:3e:5e:33:88
openshift-jwblw-master-port-0 ip_address='172.26.104.119', fa:16:3e:85:39:1d
openshift-jwblw-worker-port-2211 ip_address='172.26.104.221', fa:16:3e:dc:a2:94
"""
for line in result.split("\n"):
    if line.strip():
        parts = line.split()
        host_name = parts[0]
        ip_address = re.search(r"ip_address='(.*?)'", parts[1]).group(1)
        mac_address = parts[2]

        if "master" in host_name:
            role = "master"
            index = host_name.split("-")[-1]
        elif "worker" in host_name:
            role = "worker"
            index = host_name.split("-")[-1]
        elif "bootstrap" in host_name:
            role = "bootstrap"
            index = ""
        if role == "bootstrap":
            template_line = f"  {role}:\n    mac: \"{mac_address}\"\n    ip: \"{ip_address}\"" 
        else:
            template_line = f"  {role}-{index}:\n    mac: \"{mac_address}\"\n    ip: \"{ip_address}\""    
        template += "\n" + template_line

with open("dhcp_template.yaml", "w") as file:
    file.write(template)