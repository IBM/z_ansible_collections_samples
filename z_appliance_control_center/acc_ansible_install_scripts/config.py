#*+-------------------------------------------------------------------+
#*| IBM Confidential                                                  |
#*|                                                                   |
#*| Licensed Materials - Property of IBM                              |
#*|                                                                   |
#*|                                                                   |
#*| © Copyright IBM Corp. 2025 All Rights Reserved                    |
#*|                                                                   |
#*| The source code for this program is not published or otherwise    |
#*| divested of its trade secrets, irrespective of what has been      |
#*| deposited with the U.S. Copyright Office.                         |
#*+-------------------------------------------------------------------+
import os


CONFIG = {
    #get api token
    # "HTTP_SCHEME": "https",
    # "LOCAL_SERVER_IP": "localhost",
    # "LOCAL_SERVER_PORT": 9988,
    # "LOCAL_SSH_PORT": 9989,
    # "VERIFY_CERT": False,

    # "GATEWAY_IP": "9.152.150.165",
    # "GATEWAY_USER": "fpc-gw",
    # "HTTPS_PORT": "443",
    # "SSH_PORT": "22",
    # "D_PORT": "1234",

    # "ACTION": "activate",
    # "IS_PRIVATE": False,
    # "HMC_KEY": "~/.ssh/id_rsa",
    # "LPAR_USER": "root",
    # "LPAR_PASSWORD": "linux390",

    #only chnage this section
    # "CPC": "T28",
    # "LPAR": "SSC16",
    # "HMC_HOST": "9.152.150.166",
    # "HMC_USER": "abc@ibm.com",
    # "HMC_PASSWORD": os.environ["HMC_PASSWORD"],
    # "LPAR_IP": "9.152.150.232",
    # "disk_id": "0.0.5f46",
    # "disk_id": "0.0.1b00",
    # "IS_FCP": False, # True if disk is FCP else False
    # "lun": "4026402200000000", # To be added only when disk is FCP
    # "wwpn": "50050763070b46a6", # To be added only when disk is FCP
    # "image_path": "/Users/abc/Downloads/zacc_final_image_17_03_25.img.gz",
    # "USERNAME": "user",
    # "PASSWORD": "password",
    # "STORAGE": 16384

}

# import os
# if os.path.exists(CONFIG["image_path"]):
#     pass
# else:
#     print("Image File not found in Path")
