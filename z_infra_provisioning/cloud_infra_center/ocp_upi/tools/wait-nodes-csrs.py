#!/usr/bin/env python
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

import time
import os
import sys

work_name = sys.argv[1]

def get_worker_status():
    cmd = "./oc get nodes --kubeconfig auth/kubeconfig | grep %s | awk '{print $2}'" % work_name
    status = os.popen(cmd).read()
    if not status:
        return None
    else:
        return status.split('\n')[0:-1]

for i in range(0, 30):
    # wait up to 10 minutes to approve csrs util worker nodes' status get ready
    worker_status = get_worker_status()
    if (not worker_status) or worker_status[0] != "Ready":
        cmd = "./oc get csr -o name --kubeconfig auth/kubeconfig | xargs ./oc adm certificate approve --kubeconfig auth/kubeconfig"
        os.system(cmd)
        time.sleep(10)
    else:
        break