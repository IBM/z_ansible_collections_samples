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
from dateutil import rrule
from datetime import datetime, timedelta

total_count = sys.argv[1]
approve_node_csr = sys.argv[2]

converted_csr = int(approve_node_csr)

now = datetime.now()
hundredMinutesLater = now + timedelta(minutes=converted_csr)

def get_worker_status():
    cmd = "./oc get nodes --kubeconfig auth/kubeconfig --no-headers=true | wc -l"
    count = os.popen(cmd).read()
    if count == total_count:
        return True
    else:
        return False

for dt in rrule.rrule(rrule.MINUTELY, dtstart=now, until=hundredMinutesLater):
    worker_status = get_worker_status()
    if  worker_status == False:
        cmd = "./oc get csr -o name --kubeconfig auth/kubeconfig | xargs ./oc adm certificate approve --kubeconfig auth/kubeconfig"
        os.system(cmd)
        time.sleep(60)
    else:
        break
