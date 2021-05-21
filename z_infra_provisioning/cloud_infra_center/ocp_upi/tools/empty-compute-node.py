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

path = "install-config.yaml"
data = yaml.safe_load(open(path))
data["compute"][0]["replicas"] = 0
open(path, "w").write(yaml.dump(data, default_flow_style=False))