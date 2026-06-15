# *+------------------------------------------------------------------------+
# *| © Copyright IBM Corp. 2026                                             |
# *| [04.02.2026]                                                           |
# *|   - Tested with ACC 1.2.13                                             |
# *|   - Added network configuration for ACC partition on DPM machines      |
# *+------------------------------------------------------------------------+

import re
import traceback
import zhmcclient
from ssc_nw import get_ssc_network
def is_z17_or_greater(cpc):
        return int(cpc.get_property(
            "se-version").split(".")[1]) >= 17
def resolve_adapter(partition, nic_info):
        chpid = nic_info.get("chpid")
        fid = nic_info.get("fid")
        adapter_manager = zhmcclient.AdapterManager(partition.manager.parent)
        print(f"adapter_manager: {adapter_manager}")
        if chpid:
            args = {"channel-path-id": chpid}
        else:
            args = {"adapter-id": fid}
        print(adapter_manager.list())
        adapters = adapter_manager.list(filter_args=args)
        if len(adapters) == 0:
            print(f"Adapter not found for the input NW "
                                f"CHPID-{chpid},FID-{fid}")
            raise Exception("Adapter not found for the input NW")
        print(f"Adapter being used: {adapters[0]}")
        return adapters[0]


def get_vswitch_for_adapter(partition, adapter, port):
    vswitches = partition.manager.cpc.virtual_switches.findall(
        **{'backing-adapter-uri': adapter.uri})

    vswitch = None
    for vs in vswitches:
        if vs.get_property('port') == port:
            vswitch = vs
            break
    print(f"vswitch: {vswitch}")
    return vswitch.properties.get("object-uri")

def get_network_port_uri(adapter, port):
    port_uris = adapter.get_properties_pulled("network-port-uris")
    for port_uri in port_uris:
        adapter_port = port_uri.split('/')[-1]
        if str(port) == adapter_port:
            return port_uri
    print(
        f"Couldnt find the network port uri for port {port} in {adapter}")
    return None

def get_backing_adapter_for_nic(partition, adapter, port):
    if is_z17_or_greater(partition.manager.cpc):
        return get_network_port_uri(adapter, port)

    return get_vswitch_for_adapter(partition, adapter, port)

def create_nic(partition, nic_info):
    try:
        nic_properties = {}

        adapter_port_index = nic_info.get("port", 0)
        adapter = resolve_adapter(partition, nic_info)
        backing_uri = get_backing_adapter_for_nic(partition, adapter, adapter_port_index)
        nic_properties["name"] = "acc_mgmt_nic"
        if is_z17_or_greater(partition.manager.cpc):
            nic_properties["network-adapter-port-uri"] = backing_uri
            nic_properties["type"] = "neth" if nic_info.get("fid") else "osd"
        else:
            nic_properties['virtual-switch-uri'] = backing_uri
        nic_properties["ssc-management-nic"] = True
        nic_properties["ssc-ip-address-type"] = nic_info["static-ip-info"]["type"]
        nic_properties["ssc-ip-address"] = nic_info["static-ip-info"]["ip-address"]
        nic_properties["vlan-id"] = nic_info["vlan-id"]
        prefix = str(nic_info['static-ip-info']['prefix'])
        # if the prefix matches as 24 or 23 it adds /
        # else assumes to be in dot notation 256.255.254.0
        match = re.search("^[1-9][0-9]?$", prefix)
        if match:
            nic_properties["ssc-mask-prefix"] = f"/{prefix}"
        else:
            nic_properties["ssc-mask-prefix"] = prefix
        print(f"nic_properties: {nic_properties}")

        nic_manager = partition.nics
        nic_manager.create(nic_properties)
        print(f"created nic acc_mgmt_nic")
    except Exception as e:
        traceback.print_exc()
        print(f"Error in creating nic: {str(e)}")
        raise e

def attach_nic(partition ):
    try:
        ssc_nw_info = get_ssc_network()
        print(f"Creating nic with following config - {ssc_nw_info}")
        create_nic(partition , ssc_nw_info)
    except Exception as e:
        print(f"Error occurred while attaching nic: {str(e)}")
        raise e

def delete_all_nics(partition):
    try:
        nic_manager = partition.nics
        nics_list = nic_manager.list()
        for nic in nics_list:
            nic.delete()
    except Exception as e:
        print("failed to delete nics attached to this partition")
        raise e

