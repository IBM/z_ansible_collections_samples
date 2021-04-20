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
import sys

os_subnet_range = sys.argv[1]

path = "install-config.yaml"
data = yaml.safe_load(open(path))
data["networking"]["machineNetwork"][0]["cidr"] = os_subnet_range
open(path, "w").write(yaml.dump(data, default_flow_style=False)) 