#!/bin/bash 
# =================================================================
# Licensed Materials - Property of IBM
#
# (c) Copyright IBM Corp. 2021 All Rights Reserved
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# =================================================================

glance_endpoint=$(openstack catalog show -f json image | jq -r '.endpoints[] | "\(.url)"' | head -n 1)
image_path=$(openstack image show $1 -f json | jq -r '"\(.file)"')

image_url=$glance_endpoint$image_path
sed -i '\n'
echo $image_url