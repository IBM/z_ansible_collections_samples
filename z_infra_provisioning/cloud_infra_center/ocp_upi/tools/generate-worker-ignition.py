#!/usr/bin/env python
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

import base64
import json
import os
import sys

infra_id = sys.argv[1]
dns = sys.argv[2]

with open('worker.ign', 'r') as f:
    worker_ignition = json.load(f)

for i in range(0, 3):
    ignition = worker_ignition.copy()
    hostname = '%s-worker-%s' % (infra_id, i)
    files = []
    if 'storage' in ignition:
        files = ignition['storage'].get('files', [])
    files.append(
        {
            'path': '/etc/hostname',
            'mode': 420,
            'contents': {
                'source': 'data:text/plain;charset=utf-8;base64,' +
                base64.standard_b64encode(hostname.encode()).decode().strip(),
                'verification': {}
            },
            'filesystem': 'root'
        }
    )
    if 'storage' not in ignition:
        ignition['storage'] = dict()
    ignition['storage']['files'] = files

    with open('%s-ignition.json' % hostname, 'w') as f:
        json.dump(ignition, f)
