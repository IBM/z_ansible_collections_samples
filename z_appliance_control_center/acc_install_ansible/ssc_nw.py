# *+------------------------------------------------------------------------+
# *| © Copyright IBM Corp. 2025                                             |
# *| [04.02.2026]                                                           |
# *|   - Tested with ACC 1.2.13                                             |
# *|   - Added network configuration for ACC LPAR                           |
# *+------------------------------------------------------------------------+

import os
def get_ssc_network():
    
    nw_ip = os.environ.get("LPAR_IP")
    fid = os.environ.get("NW_FID")
    chpid = os.environ.get("NW_CHPID")
    port = os.environ.get("NW_PORT")
    prefix = os.environ.get("NW_PREFIX")
    vlan_id = os.environ.get("NW_VLANID")

    if fid:
        ssc_network = {
            'fid': fid
        }
    else:
        ssc_network = {
            'chpid': chpid,
            'port': int(port)
        }

    static_ip_info_obj = {
                        'ip-address': nw_ip,
                        'type': 'ipv4',
                        'prefix': int(prefix)
    }
    ssc_network.update({
        'ipaddr-type': "static",
        'static-ip-info': static_ip_info_obj
    })

    if vlan_id:
        ssc_network.update({'vlan-id': int(vlan_id)})
    else:
        ssc_network.update({'vlan-id': None})
    return ssc_network

