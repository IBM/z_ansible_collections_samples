#!/bin/bash 
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

for index in $(seq 0 2); do
    MASTER_HOSTNAME="$1-master-$index\n"
    python -c "import base64, json, sys
ignition = json.load(sys.stdin)
storage = ignition.get('storage', {})
files = storage.get('files', [])
files.append({'path': '/etc/hostname', 'mode': 420, 'contents': {'source': 'data:text/plain;charset=utf-8;base64,' + base64.standard_b64encode(b'$MASTER_HOSTNAME').decode().strip()},'filesystem': 'root'})
storage['files'] = files
ignition['storage'] = storage
json.dump(ignition, sys.stdout)" < master.ign > "$1-master-$index-ignition.json"
done