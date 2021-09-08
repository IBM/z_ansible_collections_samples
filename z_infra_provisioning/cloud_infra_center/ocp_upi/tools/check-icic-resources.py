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
import sys
import requests

"""
This script will get all compute hosts resources(CPU/Memory/Disk) and then
check whether OCP installation requirements are met 
""" 

nova_url = "%s/os-hypervisors/detail?include_remote_restart_enabled=true" % sys.argv[1]
storage_provider_url = "%s/storage-providers/detail" % sys.argv[2]
icic_token = sys.argv[3]
compute_nodes_num = sys.argv[4]

ca_cert_path = os.environ.get('OS_CACERT', '')
header = {
    'Content-Type': 'application/json',
    'X-Auth-Token': icic_token,
    'Accept': 'application/json',
}

usage = {
    "cpu": {"used": 0, "reserved": 0, "total": 0}, 
    "mem": {"used": 0, "reserved": 0, "total": 0},
    "disk": {"used":0, "reserved": 0, "total": 0},
    "capacity": { "used": 0, "reserved": 0, "total": 0}
}

# calculate the hypervisors resources
result = requests.get(url=nova_url, headers=header, verify=ca_cert_path)
hypervisors = result.json()["hypervisors"]
if len(hypervisors) == 0:
    sys.exit("Failed to get hypervisors resource infomartion.")
for h in hypervisors:
    if h["cpu_allocation_ratio"]:
        usage["cpu"]["total"] += h["vcpus"] * h["cpu_allocation_ratio"]    
    else:
        usage["cpu"]["total"] += h["vcpus"]
    usage["cpu"]["used"] += h["vcpus_used"]    

    if h["memory_allocation_ratio"]:
        usage["mem"]["total"] += h["memory_mb"] / 1024 * h["memory_allocation_ratio"]    
    else:
        usage["mem"]["total"] += h["memory_mb"] / 1024
    usage["mem"]["used"] += h["memory_mb_used"] / 1024    

    if h["disk_allocation_ratio"]:
        usage["disk"]["total"] += h["local_gb"] * h["disk_allocation_ratio"]    
    else:
        usage["disk"]["total"] += h["local_gb"]
    usage["disk"]["used"] += h["local_gb_used"]
    usage["disk"]["reserved"] += h["disk_available_least"]
usage["cpu"]["reserved"] = usage["cpu"]["total"] - usage["cpu"]["used"] 
usage["mem"]["reserved"] = usage["mem"]["total"] - usage["mem"]["used"] 

# calculate the storage provider capacity
result = requests.get(url=storage_provider_url, headers=header, verify=ca_cert_path)
storage_provider = result.json()["storage_providers"]
if len(storage_provider) != 0:
    for s in storage_provider:
        usage["capacity"]["total"] += s["total_capacity_gb"]
        usage["capacity"]["reserved"] += s["free_capacity_gb"]
    usage["capacity"]["used"] = usage["capacity"]["total"] - usage["capacity"]["reserved"]

# The minimum resource requirements for Openshift nodes
minimum_resources = {
    "cpu": 16 + 4 * int(compute_nodes_num),
    "mem": 64 + 16 * int(compute_nodes_num),
    "disk": 100 + 25 * int(compute_nodes_num)
}

msgs = ""
if minimum_resources["cpu"] > usage["cpu"]["reserved"]:
    msgs += "%s resources doesn't meet the minimum requirements, "\
            "current reserved: %s cpus, minimum required: %s cpus\n"\
            % ("CPU", usage["cpu"]["reserved"], minimum_resources["cpu"])
if minimum_resources["mem"] > usage["mem"]["reserved"]:
    msgs += "%s resources doesn't meet the minimum requirements, "\
            "current reserved: %s MB, minimum required: %s MB\n"\
            % ("Memory", usage["mem"]["reserved"], minimum_resources["mem"])
#if minimum_resources["disk"] > usage["disk"]["reserved"] and minimum_resources["disk"] > usage["capacity"]["reserved"]:
#   msgs += "%s resources doesn't meet the minimum requirements, "\
#          "current disk reserved: %s GB, storage provider capacity reserved: %s GB, "\
#          "minimum disk required: %s GB"\
#         % ("Storage", usage["disk"]["reserved"], usage["capacity"]["reserved"], minimum_resources["disk"])
if len(msgs) > 0:
    sys.exit(msgs)

    
