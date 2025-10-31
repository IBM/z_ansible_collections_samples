#*+-------------------------------------------------------------------+
#*| # © Copyright IBM Corp. 2025                                      |
#*| # This playbook is tested with ACC 1.2.6                          |
#*+-------------------------------------------------------------------+

"""Module for fetching API tokens."""

import os
import json
import requests

API_TOKEN_URL = "/api/com.ibm.zaci.system/api-tokens"

def get_api_token() -> tuple[int, str, str]:
    """
    Fetch API token from the server using credentials from config.py.

    Returns:
        tuple[int, str, str]: Status code, token, response text.
    """
    is_private = os.environ.get("IS_PRIVATE", "false").lower() == "true"
    if is_private:
        url = f"{os.environ.get('HTTP_SCHEME')}://{os.environ.get('LOCAL_SERVER_IP')}:{os.environ.get('LOCAL_SERVER_PORT')}{API_TOKEN_URL}"
        print(f"TOKEN URL:: {url}")
    else:
        url = f"{os.environ.get('HTTP_SCHEME')}://{os.environ.get('LPAR_IP')}{API_TOKEN_URL}"
        print(f"TOKEN URL:: {url}")
    data = {
        "kind": "request",
        "parameters": {
            "user": os.environ.get("USERNAME"),
            "password": os.environ.get("PASSWORD")
        }
    }
    data_dump = json.dumps(data)
    try:
        res = requests.post(
            url,
            data=data_dump,
            verify=os.environ.get("VERIFY_CERT", "false").lower() == "true",
            headers={
                "Accept": "application/vnd.ibm.zaci.payload+json",
                "Content-type": "application/vnd.ibm.zaci.payload+json;version=1.0",
                "zACI-API": "com.ibm.zaci.system/1.0"
            },
            timeout=10
        )
        token = ""
        print(f"Response for get API token: {res}")
        try:
            res_json = res.json()
            if "parameters" in res_json and "token" in res_json["parameters"]:
                token = res_json["parameters"]["token"]
        except requests.exceptions.JSONDecodeError:
            pass
        return (res.status_code, token, res.text)
    except requests.exceptions.RequestException as e:
        print(f"Error: {e}")
        return (500, "", "Request failed")


if __name__ == "__main__":
    rc, token, text = get_api_token()
    print(f"rc={rc}, token={token}, text={text}")
