#!/usr/bin/env python
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

import json
import sys
import hashlib
import os
import time

"""
This script will generate five-digit random numbers for compute nodes,
then store them in a json file named '.compute-nodes-{ infra-id }.json'.
The example of json file:
{"compute_node_names": ["c0526", "958ac", "1c08b"]}
"""

infra_id = sys.argv[1]
num = sys.argv[2]

compute_name_list = []

for n in range(int(num)):
    # encode timestamp with md5
    now = time.time()
    m = hashlib.md5(str(now).encode('utf-8')).hexdigest()
    compute_name_list.append(m[0:5]) 

json_file = ".compute-nodes-%s.json" % infra_id

if os.path.exists(json_file):
    # file exists, append into file
    with open(json_file, "r+") as f:
        old = f.read()
        compute_name_dict = json.loads(old)
        compute_name_dict["compute_node_names"].extend(compute_name_list)
        compute_name_json = json.dumps(compute_name_dict)
        f.seek(0)
        f.truncate()
        f.write(compute_name_json)
else:
    # file doesn't exist, construct the file 
    with open(json_file, "w") as f:
        compute_name_dict = {"compute_node_names": compute_name_list}
        compute_name_json = json.dumps(compute_name_dict)
        f.write(compute_name_json)