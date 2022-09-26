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

image_url = sys.argv[1]
token = sys.argv[2]

bootstrap_ign_shim = {
    "ignition": {
      "config": {
        "merge": [
          {
            "source": image_url,
            "httpHeaders": [
                	{
		              "name": "X-Auth-Token",
		              "value": token
	              }
              ]
          }
        ]
      },
    "version": "3.1.0"
    },
}

ca_cert_path = os.environ.get('OS_CACERT', '')
if ca_cert_path:
    with open(ca_cert_path, 'r') as f:
        ca_cert = f.read().encode()
        ca_cert_b64 = base64.standard_b64encode(ca_cert).decode().strip()
    files = bootstrap_ign_shim['ignition']
    files.update(
    {
      "security": {
        "tls": {
          "certificateAuthorities": [
            {
              "source": "data:text/plain;charset=utf-8;base64," + ca_cert_b64,
            }
          ]
        }
      }
    })

infra_id = sys.argv[3]
if infra_id:
    with open(infra_id+'-bootstrap-ignition.json', 'a') as f:
        json.dump(bootstrap_ign_shim, f)