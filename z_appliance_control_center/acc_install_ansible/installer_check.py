#!/usr/bin/env python
#*+-------------------------------------------------------------------+
#*| # © Copyright IBM Corp. 2025                                      |
#*| # This playbook is tested with ACC 1.2.6                          |
#*+-------------------------------------------------------------------+

import zhmcclient
import requests.packages.urllib3
import time
import os
import sys
import config  # Import configuration values

# Disable SSL warnings
requests.packages.urllib3.disable_warnings()

# Fetch values from config.py
HMC_HOST = os.environ.get("HMC_HOST")
HMC_USER = os.environ.get("HMC_USER")
HMC_PASSWORD = os.environ.get("HMC_PASSWORD")
CPC_NAME = os.environ.get("CPC")
LPAR_NAME = os.environ.get("LPAR")
VERIFY_CERT = os.environ.get("VERIFY_CERT", "false").lower() == "true"

# Validate required variables
if not all([HMC_HOST, HMC_USER, HMC_PASSWORD, CPC_NAME, LPAR_NAME]):
    print("❌ Error: Missing required values (HMC_HOST, HMC_USER, HMC_PASSWORD, CPC, LPAR).")
    sys.exit(1)

def hmc_status(retries=3, delay=5):
    """Checks connection to HMC and returns the client and CPC object."""
    for attempt in range(1, retries + 1):
        try:
            session = zhmcclient.Session(HMC_HOST, HMC_USER, HMC_PASSWORD, verify_cert=VERIFY_CERT)
            client = zhmcclient.Client(session)
            cpc = client.cpcs.find(name=CPC_NAME)

            print(f"DEBUG: Type of cpc: {type(cpc)}")  
            print(f"DEBUG: cpc value: {cpc}")

            print(f"✅ Successfully connected to HMC on attempt {attempt}. Found CPC: {cpc.name}\n")
            return client, cpc
        except zhmcclient.NotFound as e:
            print(f"❌ Error: {e}")
            sys.exit(1)
        except Exception as e:
            print(f"⚠️ Attempt {attempt}/{retries} failed: {e}")
            if attempt < retries:
                print(f"Retrying in {delay} seconds...\n")
                time.sleep(delay)
            else:
                print("❌ Failed to connect to HMC after multiple attempts.")
                sys.exit(1)


def lpar_status(client, cpc, lpar_name):
    """Checks the status of the LPAR and prints its mode."""
    try:
        start_time = time.time()
        if cpc.dpm_enabled:
            lpar = cpc.partitions.find(name=lpar_name)
        else:
            lpar = cpc.lpars.find(name=lpar_name)

        os_name = lpar.get_property("os-name")
        current_status = lpar.get_property("status")
        acceptable_status = lpar.get_property("acceptable-status")

        print(f"🔍 Found LPAR: {lpar.name}")
        print(f"🖥️  OS Name: {os_name}")
        print(f"📟 Current Status: {current_status}\n")
        print(f"📟 Acceptable Status: {acceptable_status[0]}\n")

        if os_name == "INSTALL" and current_status in ["operating","active"]:
            print(f"✅ LPAR '{lpar_name}' is in installer mode and operating.")
        elif os_name == "ZACC" and current_status in ["operating","active"]:
            print(f"✅ LPAR '{lpar_name}' is in Appliance mode and operating.")
        else:
            print(f"⚠️ LPAR '{lpar_name}' is not in the required state. Please check manually.")

        # Execution time check
        execution_time = time.time() - start_time
        if execution_time > 180:
            print(f"⏳ Execution is taking too long ({execution_time:.2f}s). Please manually check and refresh the HMC page.")
            sys.exit(1)
    except zhmcclient.NotFound as e:
        print(f"❌ Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Unexpected Error: {e}")
        sys.exit(1)

def main():
    """Main execution flow"""
    client, cpc = hmc_status()
    lpar_status(client, cpc)

if __name__ == "__main__":
    main()
