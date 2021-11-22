#!/bin/bash 
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================
infra_id=$1
worker_end=$(($2 - 1))

for index in $( seq 0 $worker_end); do
    WORKER_HOSTNAME="$infra_id-worker-$index\n"
    python -c "import base64, json, sys
ignition = json.load(sys.stdin)
storage = ignition.get('storage', {})
files = storage.get('files', [])
files.append({'path': '/etc/hostname', 'mode': 420, 'contents': {'source': 'data:text/plain;charset=utf-8;base64,' + base64.standard_b64encode(b'$WORKER_HOSTNAME').decode().strip()},'filesystem': 'root'})
storage['files'] = files
ignition['storage'] = storage
json.dump(ignition, sys.stdout)" < worker.ign > "$infra_id-worker-$index-ignition.json"
done