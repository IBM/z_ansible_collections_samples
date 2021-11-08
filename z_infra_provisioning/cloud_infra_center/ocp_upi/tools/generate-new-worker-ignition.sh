#!/bin/bash 
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================
WORKER_NAME=$1
WORKER_HOSTNAME="$WORKER_NAME\n"
python -c "import base64, json, sys
ignition = json.load(sys.stdin)
storage = ignition.get('storage', {})
files = storage.get('files', [])
files.append({'path': '/etc/hostname', 'mode': 420, 'contents': {'source': 'data:text/plain;charset=utf-8;base64,' + base64.standard_b64encode(b'$WORKER_HOSTNAME').decode().strip()},'filesystem': 'root'})
storage['files'] = files
ignition['storage'] = storage
json.dump(ignition, sys.stdout)" < worker.ign > "$WORKER_NAME-ignition.json"
