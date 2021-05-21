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

# Use validate environment APIs to check icic services status
# https://www.ibm.com/docs/en/cic/1.1.3?topic=apis-validate-environment

validate_init_url = "%s/v1/validate" % sys.argv[1]
validate_result_url = "%s/v1/validate/result" % sys.argv[1]
icic_token = sys.argv[2]

ca_cert_path = os.environ.get('OS_CACERT', '')
header = {
    'Content-Type': 'application/json',
    'X-Auth-Token': icic_token,
    'Accept': 'application/json',
}

# Initiate validation run
requests.get(url=validate_init_url, headers=header, verify=ca_cert_path)

# Report the results from the last completed validation
result = requests.get(url=validate_result_url, headers=header, verify=ca_cert_path)
failed_services = []
for s in result.json()["checks"]:
    if s["status"] == "fail":
        failed_services.append(s)

if len(failed_services) > 0:
    msg = "Serivces status checking failed, there are some issues: %s" % failed_services
    sys.exit(msg)
