#!/usr/bin/env python
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

import yaml

path = "manifests/cluster-scheduler-02-config.yml"
data = yaml.safe_load(open(path))
data["spec"]["mastersSchedulable"] = False
open(path, "w").write(yaml.dump(data, default_flow_style=False))