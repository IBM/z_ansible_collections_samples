#*+-------------------------------------------------------------------+
#*| # © Copyright IBM Corp. 2025                                      |
#*| # This playbook is tested with ACC 1.2.6                          |
#*+-------------------------------------------------------------------+

"""
This module automates the process of installing zFAB images to target SSC LPAR.
"""

from http import HTTPStatus
import logging
import json
from os import path
import os
import sys
import requests
import config as app_config
from api_token import get_api_token
import urllib3

urllib3.disable_warnings()

logging.basicConfig(
    format='%(asctime)s %(levelname)s:%(message)s',
    datefmt='%m/%d/%Y %I:%M:%S %p',
    level=logging.INFO
)

INSTALL_URL = "/api/com.ibm.zaci.system/sw-appliances/install"
SELECT_URL = "/api/com.ibm.zaci.system/sw-appliances/select"

def upload_zfab_image(api_token: str, disk: str, image_file: str) -> tuple[int, str]:
    """
    Upload the zFAB image to the specified disk.

    Args:
        api_token (str): The API token used for authentication.
        disk (str): The disk identifier where the image will be uploaded.
        image_file (str): The path to the image file to be uploaded.

    Returns:
        tuple[int, str]: The HTTP status code and the response text from the server.
    """
    is_private = os.environ.get("IS_PRIVATE", "false").lower() == "true"
    if is_private:
        url = f"{os.environ.get('HTTP_SCHEME')}://{os.environ.get('LOCAL_SERVER_IP')}:{os.environ.get('LOCAL_SERVER_PORT')}{INSTALL_URL}?id={disk}"
        print(f"UPLOAD URL:: {url}")
    else:
        url = f"{os.environ['HTTP_SCHEME']}://{os.environ['LPAR_IP']}{INSTALL_URL}?id={disk}"
        print(f"UPLOAD URL:: {url}")

    # For FCP disk
    is_fcp = os.environ.get("IS_FCP", "false").lower() == "true"
    if is_fcp:
        lun = os.environ.get("lun")
        wwpn = os.environ.get("wwpn")
        if not lun or not wwpn:
            raise ValueError("Both 'lun' and 'wwpn' environment variables must be set for FCP disk.")
        url += f"&lun={lun}&wwpn={wwpn}"
        print(f"UPLOAD URL for FCP disk:: {url}")

    with open(image_file, 'rb') as payload:
        res = requests.post(
            url, 
            data=payload, 
            verify=os.environ.get("VERIFY_CERT", "false").lower() == "true",
            headers={
                "Accept": "application/vnd.ibm.zaci.payload+json;version=1.0",
                'Content-type': "application/octet-stream",
                'Zaci-Api': 'com.ibm.zaci.system/1.0',
                'Authorization': f"Bearer {api_token}"
            }, 
            # increased the timeout for upload image to 40 mins for safer side
            timeout=40*60
        )
    return (res.status_code, res.text)

def select(api_token: str, disk: str, lun: str=None, wwpn: str=None) -> tuple[int, str]:
    """
    Select the disk where the image will be installed.

    Args:
        api_token (str): The API token used for authentication.
        disk (str): The disk identifier to select for image installation.

    Returns:
        tuple[int, str]: The HTTP status code and the response text from the server.
    """
    is_private = os.environ.get("IS_PRIVATE", "false").lower() == "true"
    if is_private:
        url = f"{os.environ.get('HTTP_SCHEME')}://{os.environ.get('LOCAL_SERVER_IP')}:{os.environ.get('LOCAL_SERVER_PORT')}{SELECT_URL}"
        print(f"SELECT DISK URL:: {url}")
    else:
        url = f"{os.environ['HTTP_SCHEME']}://{os.environ['LPAR_IP']}{SELECT_URL}"
        print(f"SELECT DISK URL:: {url}")

    data = {
        "kind": "request",
        "parameters": {
            "disk": {
                "capacity": "",
                "id": disk,
                "progress": {},
                "self": f"/api/com.ibm.zaci.system/storage-devices/{disk}",
                "status": "free",
                "type": "3390/0c"
            },
            "reboot-after": True
        }
    }
    if lun and wwpn:
        data["parameters"]["disk"]["lun"] = lun
        data["parameters"]["disk"]["wwpn"] = wwpn
        data["parameters"]["disk"]["type"] = "FCP"
    data_dump = json.dumps(data)

    # regenerate the token - token creation also before select operation to avoid such usecases
    rc, latest_token, _ = get_api_token()
        #print(token)
    if rc != HTTPStatus.OK:
        print(f"Bad rc from get_api_token: {rc}")
        sys.exit(1)
    print("Successfully regenerated API token")

    res = requests.put(
        url, 
        data=data_dump, 
        verify=os.environ.get("VERIFY_CERT", "false").lower() == "true",
        headers={
            "Accept": "application/vnd.ibm.zaci.payload+json;version=1.0",
            'Content-type': "application/vnd.ibm.zaci.payload+json;version=1.0",
            'Zaci-Api': 'com.ibm.zaci.system/1.0',
            'Authorization': f"Bearer {latest_token}"
        }, 
        timeout=120
    )
    return (res.status_code, res.text)
