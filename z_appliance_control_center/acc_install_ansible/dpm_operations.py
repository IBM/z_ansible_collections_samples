#*+-------------------------------------------------------------------+
#*| # © Copyright IBM Corp. 2025                                      |
#*| # This playbook is tested with ACC 1.2.6                          |
#*+-------------------------------------------------------------------+

import sys
import time
import os
import config as app_config
import zhmcclient
import urllib3

urllib3.disable_warnings()
NOT_ACTIVATED_STATUS = 'stopped'
OPERATING_STATUS = 'active'
EXCEPTIONS_STATUS = 'exceptions'

ACTION_ACTIVATE = "activate"
ACTION_DEACTIVATE = "deactivate"

def main(session,set_image_profile):
    try:
        cpc_name = os.environ.get("CPC")
        lpar_name = os.environ.get("LPAR")
        action = os.environ.get("ACTION")       

        if action not in [ACTION_ACTIVATE, ACTION_DEACTIVATE]:
            print(f"Unsupported action: {action}")
            sys.exit(1)

        print(f"Attempting to {action} CPC {cpc_name} LPAR {lpar_name}...")
        cpc, lpar = get_cpc_lpar(session, cpc_name, lpar_name)
        status = lpar.get_property('status')
        print(f"LPAR {lpar_name} current status: {status}")

        if action == ACTION_ACTIVATE:
            if status == OPERATING_STATUS:
                if set_image_profile:
                    print(f"LPAR {lpar_name} is already active. Deactivating and reactivating...")
                    if deactivate_lpar(cpc, lpar):
                        update_partition_properties(cpc, lpar_name)
                        time.sleep(10)
                    activate_lpar(cpc, lpar)
                    print(f"CPC {cpc_name} LPAR {lpar_name} has been successfully reactivated...and switched to Installer Mode")
                else: 
                    print(f"LPAR {lpar_name} is already active.")
            elif status == NOT_ACTIVATED_STATUS:
                if set_image_profile:
                    print(f"LPAR {lpar_name} is inactive. Activating with Installer mode...")
                    update_partition_properties(cpc, lpar_name)
                    activate_lpar(cpc, lpar)
                    print(f"CPC {cpc_name} LPAR {lpar_name} has been successfully activated.")
                else:
                    print(f"LPAR {lpar_name} is inactive. Activating with same previous image...")
                    activate_lpar(cpc, lpar)
                    print(f"CPC {cpc_name} LPAR {lpar_name} has been successfully activated.")
            else:
                print(f"LPAR {lpar_name} is in an unexpected state: {status}. Attempting deactivation and reactivation and setting to Installer mode...")
                if deactivate_lpar(cpc, lpar):
                    update_partition_properties(cpc, lpar_name)
                    time.sleep(10)
                activate_lpar(cpc, lpar)
                print(f"CPC {cpc_name} LPAR {lpar_name} has been successfully reactivated.")
        elif action == ACTION_DEACTIVATE:
            if status != NOT_ACTIVATED_STATUS:
                print(f"Deactivating LPAR {lpar_name}...")
                deactivate_lpar(cpc, lpar)
                print(f"CPC {cpc_name} LPAR {lpar_name} has been successfully deactivated.")
            else:
                print(f"LPAR {lpar_name} is already inactive.")
    except Exception as e:
        print(f"Error: {e}")
        raise
    finally:
        session.logoff()
        print("Logged off from HMC")

def get_cpc_lpar(session, cpc: str, lpar: str):
    client = zhmcclient.Client(session)
    cpcs = client.cpcs.list(filter_args={'name': cpc})
    if len(cpcs) != 1:
        raise Exception(f"Expected 1 CPC, but got {len(cpcs)}.")
    cpc = cpcs[0]
    print(f"Machine type: {cpc.get_property('machine-type')}")

    partitions = cpc.partitions.list(filter_args={'name': lpar})
    if len(partitions) != 1:
        raise Exception(f"Expected 1 LPAR, but got {len(partitions)}.")
    lpar = partitions[0]
    return cpc, lpar

def activate_lpar(cpc, lpar, retries: int = 10, interval: int = 2):
    print(f"Activating LPAR {lpar.get_property('name')}...")
    lpar.start(wait_for_completion=True)
    for attempt in range(retries):
        lpar = cpc.partitions.list(filter_args={'name': lpar.get_property('name')})[0]
        status = lpar.get_property('status')
        if status == OPERATING_STATUS:
            print(f"LPAR {lpar.get_property('name')} is now active.")
            return True  #Indicate success
        print(f"Activation attempt {attempt + 1}/{retries} failed. Retrying in {interval} seconds...")
        time.sleep(interval)
    raise Exception(f"Failed to activate LPAR {lpar.get_property('name')} after {retries} attempts.")


def deactivate_lpar(cpc, lpar, retries: int = 10, interval: int = 2):
    status = lpar.get_property('status')
    if status == NOT_ACTIVATED_STATUS:
        print(f"LPAR {lpar.get_property('name')} is already inactive. Skipping deactivation.")
        return False  #No action was needed

    print(f"Deactivating LPAR {lpar.get_property('name')}...")
    lpar.stop(wait_for_completion=True)  
    for attempt in range(retries):
        lpar = cpc.partitions.list(filter_args={'name': lpar.get_property('name')})[0]
        status = lpar.get_property('status')
        if status == NOT_ACTIVATED_STATUS:
            print(f"LPAR {lpar.get_property('name')} is now inactive.")
            return True  #Deactivation successful
        print(f"Deactivation attempt {attempt + 1}/{retries} failed. Retrying in {interval} seconds...")
        time.sleep(interval)
    raise Exception(f"Failed to deactivate LPAR {lpar.get_property('name')} after {retries} attempts.")

def update_partition_properties(cpc, lpar_name: str):
    print(f"Setting partition properties for LPAR {lpar_name}...")
    lpar_list = cpc.partitions.list(filter_args={'name': lpar_name})
    if len(lpar_list) != 1:
        raise Exception(f"Unexpected number of profiles returned: {len(lpar_list)}.")
    lpar_obj = lpar_list[0]
    print(f"profile : {lpar_obj}")
    #profile.update_properties({'ssc-boot-selection': 'installer'})
    storage = int(os.environ.get("STORAGE", 0))
    user = os.environ.get("USERNAME")
    password = os.environ.get("PASSWORD")
    lpar_obj.update_properties({'ssc-boot-selection': 'installer',
                                'initial-memory': storage,
                                'ssc-master-userid':user,
                                'ssc-master-pw':password}) #sending central storage, value should be number 16384
    print(f"Partition properties set for LPAR {lpar_name}.")
