# *+------------------------------------------------------------------------+
# *| © Copyright IBM Corp. 2025                                             |
# *| [10.17.2025]                                                           |
# *|   - Tested with ACC 1.2.6                                              |
# *|   - Initial release                                                    |
# *| [12.12.2025]                                                           |
# *|   - Tested with ACC 1.2.10                                             |
# *+------------------------------------------------------------------------+

import subprocess
import shlex
import os

import requests
# from config import CONFIG

IS_OPERATIONAL_URL = '/api/com.ibm.zaci.system/appliance/is-operational'


def login_to_lpar():
    """
    Open an interactive SSH session with the LPAR.
    """
    local_ssh_port = int(os.environ.get("LOCAL_SSH_PORT"))
    ssh_command = f'sshpass -p {os.environ.get("LPAR_PASSWORD")} ssh -p {local_ssh_port} {os.environ.get("LPAR_USER")}@{os.environ.get("LOCAL_SERVER_IP")} ' \
                  f'-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null '

    try:
        subprocess.run(shlex.split(ssh_command))  #Open an interactive shell session
        #subprocess.call(shlex.split(ssh_command))  #Keeps the session open until user exits
        return 0, "Logged in successfully.", ""
    except Exception as e:
        return -1, "", str(e)


def check_lpar_is_operational() -> tuple[int, str]:
    """
    Check LPAR status is operational.

    Returns:
        tuple[int, str]: Status code, response text.
    """
    is_private = os.environ.get("IS_PRIVATE", "false").lower() == "true"
    local_server_port = int(os.environ.get("LOCAL_SERVER_PORT"))
    if is_private:
        url = f"{os.environ.get('HTTP_SCHEME')}://{os.environ.get('LOCAL_SERVER_IP')}:{local_server_port}{IS_OPERATIONAL_URL}"
    else:
        url = f"{os.environ.get('HTTP_SCHEME')}://{os.environ.get('LPAR_IP')}{IS_OPERATIONAL_URL}"

    print(f"URL for is_operation API: {url}")
    try:
        res = requests.get(
            url,
            verify=os.environ.get("VERIFY_CERT", "false").lower() == "true",
            headers={
                "Accept": "application/vnd.ibm.zaci.payload+json",
                "Content-type": "application/vnd.ibm.zaci.payload+json;version=1.0",
                "zACI-API": "com.ibm.zaci.system/1.0"
            },
            timeout=10
        )
        return (res.status_code, res.text)
    except requests.exceptions.RequestException as e:
        # print(f"Error: {e}")
        return (500, str(e))
