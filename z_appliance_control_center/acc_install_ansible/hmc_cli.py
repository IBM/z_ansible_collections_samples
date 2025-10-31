#*+-------------------------------------------------------------------+
#*| # © Copyright IBM Corp. 2025                                      |
#*| # This playbook is tested with ACC 1.2.6                          |
#*+-------------------------------------------------------------------+

import random
import string
import click
from click_shell import shell
# import config as app_config
from installer_check import *
from activate_deactivate import *
from activate_deactivate import main as classic_operations
from dpm_operations import main as dpm_operations
from upload_image import upload_zfab_image, select
import connection
from api_token import get_api_token
import commands
import re
import os
from os import path
import zhmcclient
import logging
from http import HTTPStatus
import shlex
import subprocess

#steps:
#python3 -m venv myenv
#source myenv/bin/activate
#pip3 install click
#pip3 install click_shell
#delete disconencted sessions if any issues while loading

CONFIG_FILE = os.path.abspath("config.py")

@shell(intro='Entering HMC mode. Type "exit" to quit.')
def hmc():
    """Starts the installer process"""
    click.echo("Automatically invoking 'hard' command...")
    hard()

###############################################
@hmc.group(name="tunnel")  #renamed the Click group to avoid conflict
def tunnel():
    """Initializing tunneling..."""
    pass

@tunnel.command()
def start():
    """Starts tunneling..."""
    click.echo("Starting tunneling...")
    connection.start_tunneling() 

@tunnel.command()
def stop():
    """Stops tunneling..."""
    click.echo("Stopping tunneling...")
    connection.stop_tunneling() 

@tunnel.command()
def status():
    """Checks if tunneling is active."""
    if connection.is_tunnel_active():
        click.echo("SSH tunnel is active ✅")
    else:
        click.echo("SSH tunnel is inactive ❌")

###############################################

@hmc.command()
def status():
    """Shows HMC connection status"""
    click.echo("Fetching HMC status...\n")
    try:
        hmc_status() 
    except Exception as e:
        click.echo(f"Error checking HMC status: {e}")

###############################################

@hmc.group()
def config():
    """Manage configuration settings"""
    pass

# @config.command()
# def show():
#     """Display configuration settings"""
#     for key, value in app_config.CONFIG.items():
#         # if key=="HMC_PASSWORD":
#         if "PASSWORD" in key:
#             click.echo(f"{key}={generate_masked_password()}")
#         else:
#             click.echo(f"{key}={value}")

# @config.command()
# @click.option('--LPAR', help="Set the LPAR value")
# @click.option('--CPC', help="Set the CPC value")
# @click.option('--ACTION', help="Set the ACTION value")
# @click.option('--HMC_HOST', help="Set the HMC_HOST value")
# def edit(lpar, cpc, action, hmc_host):
#     """
#     Edit configuration settings and persist them to file dynamically.

#     Example Usage:
#       hmc> config edit --LPAR='SSC17'
#     """
#     changes = {}

#     if lpar:
#         app_config.CONFIG["LPAR"] = lpar
#         changes["LPAR"] = lpar
#     if cpc:
#         app_config.CONFIG["CPC"] = cpc
#         changes["CPC"] = cpc
#     if action:
#         app_config.CONFIG["ACTION"] = action
#         changes["ACTION"] = action
#     if hmc_host:
#         app_config.CONFIG["HMC_HOST"] = hmc_host
#         changes["HMC_HOST"] = hmc_host

#     if changes:
#         update_config_file(changes)
#         click.echo("✅ Configuration updated in file!")
#     else:
#         click.echo("⚠️ No changes made.")

# def update_config_file(changes):
#     """Update the config.py file with new values"""
#     with open(CONFIG_FILE, "r") as file:
#         lines = file.readlines()

#     with open(CONFIG_FILE, "w") as file:
#         for line in lines:
#             match = re.match(r'^\s*"(\w+)"\s*:\s*"(.*?)",?', line)  # Match key-value pairs
#             if match:
#                 key = match.group(1)
#                 if key in changes:
#                     line = f'    "{key}": "{changes[key]}",\n'
#             file.write(line)

def generate_masked_password(length=12):
    characters = string.ascii_letters + string.digits + string.punctuation
    pwd_mask = ''.join(random.choice(characters) for _ in range(length))
    return pwd_mask

###############################################

@hmc.group()
def installer():
    """Switch between installer and appliance mode"""
    pass

@installer.command()
def soft():
    """Switch from appliance to installer mode softly"""
    click.echo("Soft Insattler Mode, which is switching from Appliance to Installer Mode...")
    installer_action("activate")

@installer.command()
def hard():
    """Remove existing image and switch to installer mode"""
    click.echo("Switching to installer mode with image removal...")
    image_action("activate")
    image_task()
    click.echo("Image task is done succesfully ✅")
    click.echo("Invoking lpar boot status...")
    check_lpar_boot_status()

def installer_action(action, set_image_profile=True):
    """Helper function to execute LPAR actions"""
    try:
        userid = os.environ.get("HMC_USER")
        password = os.environ.get("HMC_PASSWORD")
        host = os.environ.get("HMC_HOST")
        verify_cert = False
        session = zhmcclient.Session(host, userid, password, verify_cert=verify_cert)
        os.environ["ACTION"] = action
        main(session, set_image_profile)
    except Exception as e:
        print(f"Unhandled exception occurred in installer_action : {e}")
        sys.exit(1)

def image_action(action, set_image_profile=True):
    """Helper function to execute LPAR actions"""
    try:
        userid = os.environ.get("HMC_USER")
        password = os.environ.get("HMC_PASSWORD")
        host = os.environ.get("HMC_HOST")
        verify_cert = False
        session = zhmcclient.Session(host, userid, password, verify_cert=verify_cert)
        os.environ["ACTION"] = action
        initiate_action(session,set_image_profile)
    except Exception as e:
        print(f"Unhandled exception occurred in image_action: {e}")
        sys.exit(1)

def initiate_action(session, set_image_profile):
    cpc_name = os.environ.get("CPC")
    lpar_name = os.environ.get("LPAR")
    action = os.environ.get("ACTION")       
    print(f"CPC Name: {cpc_name}")
    print(f"LPAR Name: {lpar_name}")
    print(f"Action: {action}")
    print("Image Activation Profile:",set_image_profile)
    cpc = get_cpc(session, cpc_name)
    if cpc.dpm_enabled:
        dpm_operations(session,set_image_profile)
    else:
        classic_operations(session, set_image_profile)

def get_cpc(session, cpc):
    client = zhmcclient.Client(session)
    cpcs = client.cpcs.list(filter_args={'name': cpc})
    print(cpcs)
    if len(cpcs) != 1:
        raise Exception(f"Expected 1 CPC, but got {len(cpcs)}.")
    cpc = cpcs[0]
    print(f"Machine type: {cpc.get_property('machine-type')}")
    print(f"Machine type - DPM : {cpc.dpm_enabled}")
    return cpc

def image_task():
    """Helper function to execute LPAR actions"""
    try:
        param_image_file = os.environ.get("IMAGE_PATH")
        param_disk = os.environ.get("DISK_ID")
        print(f"Disk ID and Image File: {param_image_file, param_disk}")

        if not path.isfile(param_image_file):
            print(f"{param_image_file} is not a valid file")
            sys.exit(1)

        rc, token, _ = get_api_token()
        #print(token)
        if rc != HTTPStatus.OK:
            print(f"Bad rc from get_api_token: {rc}")
            sys.exit(1)
        
        print("Successfully received API token")

        print("Start uploading zFAB image, this may take several minutes")
        rc, text = upload_zfab_image(token, param_disk, param_image_file)
        print(f"Upload zFAB image rc={rc}, response text={text}")
        if rc != HTTPStatus.NO_CONTENT:
            sys.exit(1)

        if os.environ.get('IS_FCP').lower()=="true":
            lun = os.environ.get("lun")
            wwpn = os.environ.get("wwpn")
            rc, text = select(token, param_disk, lun, wwpn)
        else:
            rc, text = select(token, param_disk)
        print(f"Select rc={rc}, response text={text}")
        if rc != HTTPStatus.ACCEPTED:
            sys.exit(1)
    except Exception as e:
        print(f"Unhandled exception occurred in image_task: {e}")
        sys.exit(1)


def check_lpar_boot_status():
    """
    Checks for the status of LPAR post image upload with polling every 10 seconds for up to 15 minutes
    """

    click.echo("Performing lpar is operational status check...")

    # Set the polling interval and maximum duration
    polling_interval = 10  # seconds
    max_duration = 15 * 60  # 15 minutes in seconds
    start_time = time.time()

    while True:
        click.echo("Check LPAR boot status function started...")
        rc, _ = commands.check_lpar_is_operational()
        if rc == 204:
            click.echo("System is operational ✅")
            click.echo("Check LPAR status...🔍")
            status()
            click.echo("ACC installation completed ✅")
            break # Break the loop once the LPAR is operational
        
        # Check if the maximum duration has passed
        elapsed_time = time.time() - start_time
        if elapsed_time >= max_duration:
            click.echo("⚠️ Abnormal system behavior detected after 15 minutes of polling! Please check manually. ⚠️")
            break # Break the loop once the timeout is reached
        
        time.sleep(polling_interval)

###############################################

@hmc.group()
def lpar():
    """Manage LPARs"""
    pass

@lpar.command()
def status():
    """Shows LPAR status"""
    click.echo("Fetching LPAR status...\n")

    try:
        CPC_NAME = os.environ.get("CPC")
        LPAR_NAME = os.environ.get("LPAR")

        if not CPC_NAME or not LPAR_NAME:
            click.echo("❌ Error: Missing CPC or LPAR name in config.")
            return

        client, cpc = hmc_status()
        if not client or not cpc:
            click.echo("❌ HMC connection failed.")
            return

        click.echo(f"✅ Found CPC: {CPC_NAME}")
        click.echo(f"🔍 Checking status for LPAR: {LPAR_NAME}")

        lpar_status(client, cpc, LPAR_NAME)

    except AttributeError as e:
        click.echo(f"❌ Config Attribute Error: {e}")
    except Exception as e:
        click.echo(f"❌ Error checking LPAR status: {e}")


@lpar.command()
def activate():
    """Activate LPAR"""
    click.echo("Activating LPAR...")
    execute_action("activate")

@lpar.command()
def deactivate():
    """Deactivate LPAR"""
    click.echo("Deactivating LPAR...")
    execute_action("deactivate")

def execute_action(action, set_image_profile=False):
    """Helper function to execute LPAR actions"""
    try:
        userid = os.environ.get("HMC_USER")
        password = os.environ.get("HMC_PASSWORD")
        host = os.environ.get("HMC_HOST")
        verify_cert = False
        session = zhmcclient.Session(host, userid, password, verify_cert=verify_cert)
        os.environ["ACTION"] = action
        main(session, set_image_profile)
    except Exception as e:
        print(f"Unhandled exception occurred in execute_action: {e}")
        sys.exit(1)


@lpar.command()
def login():
    """Login into Remote LPAR"""
    click.echo("Logging into LPAR...")
    
    rc, stdout, stderr = commands.login_to_lpar()

    if rc == 0:
        click.echo(stdout)
    else:
        click.echo(f"Error: {stderr}", err=True)

###############################################


if __name__ == '__main__':
    hmc()
