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
master_end=$(($2 - 1))
name_prefix=$3

for index in $( seq 0 $master_end); do
    if [ -n "$name_prefix" ]; then
        MASTER_HOSTNAME="$name_prefix-$index\n"
        IGNITION_NAME="$name_prefix-$index"
    else
        MASTER_HOSTNAME="$infra_id-master-$index\n"
        IGNITION_NAME="$infra_id-master-$index"
    fi
    python -c "import base64, json, sys
ignition = json.load(sys.stdin)
storage = ignition.get('storage', {})
files = storage.get('files', [])
files.append({'path': '/etc/hostname', 'mode': 420, 'contents': {'source': 'data:text/plain;charset=utf-8;base64,' + base64.standard_b64encode(b'$MASTER_HOSTNAME').decode().strip()},'filesystem': 'root'})
storage['files'] = files
ignition['storage'] = storage
json.dump(ignition, sys.stdout)" < master.ign > "$IGNITION_NAME-ignition.json"
done