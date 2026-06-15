#!/bin/bash
# *+------------------------------------------------------------------------+
# *| © Copyright IBM Corp. 2026                                             |
# *| [04.02.2026]                                                           |
# *|   - Tested with ACC 1.2.13                                             |
#* |   - Initial release                                                    |
# *+------------------------------------------------------------------------+

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored messages
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if jq is installed (required for JSON parsing)
if ! command -v jq >/dev/null 2>&1; then
    print_error "'jq' is not installed. Please install it to parse JSON responses."
    echo "Installation instructions:"
    echo "  - macOS: brew install jq"
    echo "  - Ubuntu/Debian: sudo apt-get install jq"
    echo "  - RHEL/CentOS: sudo yum install jq"
    exit 1
fi

# Validate required environment variables
print_info "Validating environment variables..."

required_vars=(
    "LPAR_IP"
    "IMAGE_PATH"
    "DISK_ID"
    "USERNAME"
    "PASSWORD"
    "IS_FCP"
)

missing_vars=()
for var_name in "${required_vars[@]}"; do
    if [ -z "${!var_name}" ]; then
        missing_vars+=("$var_name")
    fi
done

if [ ${#missing_vars[@]} -ne 0 ]; then
    print_error "Missing required environment variables:"
    for var in "${missing_vars[@]}"; do
        echo "  - $var"
    done
    echo ""
    echo "Please set the required environment variables before running this script:"
    echo "  export LPAR_IP=<lpar_ip_address>"
    echo "  export IMAGE_PATH=<path_to_image_file>"
    echo "  export DISK_ID=<disk_id>"
    echo "  export USERNAME=<username>"
    echo "  export PASSWORD=<password>"
    echo "  export IS_FCP=<true|false>"
    exit 1
fi

# Additional validation for FCP-specific variables
if [ "$IS_FCP" = "true" ]; then
    print_info "FCP mode detected. Validating FCP-specific variables..."
    fcp_vars=("lun" "wwpn")
    missing_fcp_vars=()
    
    for var_name in "${fcp_vars[@]}"; do
        if [ -z "${!var_name}" ]; then
            missing_fcp_vars+=("$var_name")
        fi
    done
    
    if [ ${#missing_fcp_vars[@]} -ne 0 ]; then
        print_error "IS_FCP is true but missing required FCP variables:"
        for var in "${missing_fcp_vars[@]}"; do
            echo "  - $var"
        done
        echo ""
        echo "Please set the required FCP environment variables:"
        echo "  export lun=<lun_value>"
        echo "  export wwpn=<wwpn_value>"
        exit 1
    fi
    
    print_info "FCP variables validated successfully."
    print_info "LUN: $lun"
    print_info "WWPN: $wwpn"
fi

# Validate image file exists
if [ ! -f "$IMAGE_PATH" ]; then
    print_error "Image file not found: $IMAGE_PATH"
    exit 1
fi

print_info "All required environment variables are set."
print_info "LPAR IP: $LPAR_IP"
print_info "Username: $USERNAME"
print_info "Image Path: $IMAGE_PATH"
print_info "Disk ID: $DISK_ID"
print_info "IS_FCP: $IS_FCP"
if [ "$IS_FCP" = "true" ]; then
    print_info "LUN: $lun"
    print_info "WWPN: $wwpn"
fi
echo ""

# CURL COMMAND 1: Get authentication token
print_info "CURL 1: Authenticating and retrieving token..."

token_response=$(curl -k -s -w "\n%{http_code}" -X POST \
    "https://${LPAR_IP}/api/com.ibm.zaci.system/api-tokens" \
    -H "Accept:application/vnd.ibm.zaci.payload+json" \
    -H "Content-type:application/vnd.ibm.zaci.payload+json;version=1.0" \
    -H "zACI-API: com.ibm.zaci.system/1.0" \
    -d "{\"kind\": \"request\", \"parameters\": {\"user\":\"${USERNAME}\", \"password\": \"${PASSWORD}\"}}")

# Check if curl command was successful
if [ $? -ne 0 ]; then
    print_error "Failed to connect to API at https://${LPAR_IP}"
    exit 1
fi

# Extract HTTP status code (last line) and response body
http_code=$(echo "$token_response" | tail -n1)
response_body=$(echo "$token_response" | sed '$d')

# Check HTTP status code
if [ "$http_code" != "200" ]; then
    print_error "Authentication failed with HTTP status code: $http_code"
    print_error "Response: $response_body"
    exit 1
fi

# Extract token from response
# Response format: {"kind": "response", "parameters": {"token": "...", "isAdmin": true}}
TOKEN=$(echo "$response_body" | jq -r '.parameters.token // empty')

if [ -z "$TOKEN" ] || [ "$TOKEN" == "null" ]; then
    print_error "Failed to extract token from response."
    print_error "Response received: $response_body"
    exit 1
fi

print_info "Token retrieved successfully (HTTP 200)."
print_info "Token: ${TOKEN:0:50}..." # Show only first 50 characters for security
echo ""

# CURL COMMAND 2: Install appliance with image upload
print_info "CURL 2: Installing appliance and uploading image..."

# Build install URL with query parameters based on IS_FCP value
if [ "$IS_FCP" = "true" ]; then
    print_info "Using FCP disk configuration for installation..."
    install_url="https://${LPAR_IP}/api/com.ibm.zaci.system/sw-appliances/install?id=${DISK_ID}&lun=${lun}&wwpn=${wwpn}"
else
    print_info "Using standard disk configuration for installation..."
    install_url="https://${LPAR_IP}/api/com.ibm.zaci.system/sw-appliances/install?id=${DISK_ID}"
fi

second_response=$(curl -k -s -w "\n%{http_code}" -X "POST" \
    "$install_url" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H "Accept: application/vnd.ibm.zaci.payload+json;version=1.0" \
    -H "Content-type: application/octet-stream" \
    -H "Transfer-Encoding: chunked" \
    -H "Zaci-Api: com.ibm.zaci.system/1.0" \
    -T "${IMAGE_PATH}")

if [ $? -ne 0 ]; then
    print_error "Image installation failed."
    exit 1
fi

# Extract HTTP status code and response body
http_code=$(echo "$second_response" | tail -n1)
response_body=$(echo "$second_response" | sed '$d')

# Check HTTP status code (accept 204)
if [ "$http_code" != "204" ]; then
    print_error "Image installation failed with HTTP status code: $http_code"
    print_error "Response: $response_body"
    exit 1
fi

print_info "Image installation done successfully (HTTP $http_code)."
print_info "Response: $response_body"
echo ""

# CURL COMMAND 3: Select appliance and reboot
print_info "CURL 3: Selecting appliance and initiating reboot..."

# Build JSON payload based on IS_FCP value
if [ "$IS_FCP" = "true" ]; then
    print_info "Using FCP disk configuration with LUN and WWPN..."
    json_payload="{\"kind\": \"request\",\"parameters\": {\"disk\": { \"id\": \"${DISK_ID}\", \"lun\": \"${lun}\", \"wwpn\": \"${wwpn}\"}, \"reboot-after\": true}}"
else
    print_info "Using standard disk configuration..."
    json_payload="{\"kind\": \"request\",\"parameters\": {\"disk\": { \"id\": \"${DISK_ID}\"}, \"reboot-after\": true}}"
fi

# Refresh the token before appliance selection
print_info "Refreshing authentication token..."

refresh_response=$(curl -k -s -w "\n%{http_code}" -X POST \
    "https://${LPAR_IP}/api/com.ibm.zaci.system/api-tokens" \
    -H "Accept:application/vnd.ibm.zaci.payload+json" \
    -H "Content-type:application/vnd.ibm.zaci.payload+json;version=1.0" \
    -H "zACI-API: com.ibm.zaci.system/1.0" \
    -d "{\"kind\": \"request\", \"parameters\": {\"user\":\"${USERNAME}\", \"password\": \"${PASSWORD}\"}}")

# Check if curl command was successful
if [ $? -ne 0 ]; then
    print_error "Failed to refresh token"
    exit 1
fi

# Extract HTTP status code and response body
http_code=$(echo "$refresh_response" | tail -n1)
response_body=$(echo "$refresh_response" | sed '$d')

# Check HTTP status code
if [ "$http_code" != "200" ]; then
    print_error "Token refresh failed with HTTP status code: $http_code"
    print_error "Response: $response_body"
    exit 1
fi

# Extract new token from response
NEW_TOKEN=$(echo "$response_body" | jq -r '.parameters.token // empty')

if [ -z "$NEW_TOKEN" ] || [ "$NEW_TOKEN" == "null" ]; then
    print_error "Failed to extract refreshed token from response."
    print_error "Response received: $response_body"
    exit 1
fi

TOKEN="$NEW_TOKEN"
print_info "Token refreshed successfully."
print_info "New Token: ${TOKEN:0:50}..."
echo ""

third_response=$(curl -k -s -w "\n%{http_code}" -X "PUT" \
    "https://${LPAR_IP}/api/com.ibm.zaci.system/sw-appliances/select" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H "Accept: application/vnd.ibm.zaci.payload+json;version=1.0" \
    -H "Content-type: application/vnd.ibm.zaci.payload+json;version=1.0" \
    -H "Zaci-Api: com.ibm.zaci.system/1.0" \
    -d "$json_payload")

if [ $? -ne 0 ]; then
    print_error "Selecting the disk to boot the installed appliance failed."
    exit 1
fi

# Extract HTTP status code and response body
http_code=$(echo "$third_response" | tail -n1)
response_body=$(echo "$third_response" | sed '$d')

# Check HTTP status code (accept 204 or 202 )
if [ "$http_code" != "204" ] && [ "$http_code" != "202" ]; then
    print_error "Appliance selection failed with HTTP status code: $http_code"
    print_error "Response: $response_body"
    exit 1
fi

print_info "Appliance boot initiated successfully (HTTP $http_code)."
print_info "Response:"
echo "$response_body" | jq '.'
# CURL COMMAND 4: Wait for system to boot and become reachable
print_info "CURL 4: Waiting for system to boot and become reachable..."
print_info "Polling token API every 40 seconds (timeout: 30 minutes)..."

# Calculate timeout (30 minutes = 1800 seconds)
TIMEOUT=1800
SLEEP_INTERVAL=40
elapsed_time=0
boot_successful=false

while [ $elapsed_time -lt $TIMEOUT ]; do
    # Calculate remaining time
    remaining=$((TIMEOUT - elapsed_time))
    minutes=$((remaining / 60))
    seconds=$((remaining % 60))
    
    print_info "Attempt at ${elapsed_time}s (${minutes}m ${seconds}s remaining)..."
    
    # Try to get a new token to check if system is up
    boot_check_response=$(curl -k -s -w "\n%{http_code}" -X POST \
        "https://${LPAR_IP}/api/com.ibm.zaci.system/api-tokens" \
        -H "Accept:application/vnd.ibm.zaci.payload+json" \
        -H "Content-type:application/vnd.ibm.zaci.payload+json;version=1.0" \
        -H "zACI-API: com.ibm.zaci.system/1.0" \
        -d "{\"kind\": \"request\", \"parameters\": {\"user\":\"${USERNAME}\", \"password\": \"${PASSWORD}\"}}" 2>/dev/null)
    
    # Extract HTTP status code (last line)
    http_code=$(echo "$boot_check_response" | tail -n1)
    response_body=$(echo "$boot_check_response" | sed '$d')
    
    # Check if we got a successful response
    if [ "$http_code" = "200" ]; then
        # Try to extract token to verify it's a valid response
        NEW_TOKEN=$(echo "$response_body" | jq -r '.parameters.token // empty' 2>/dev/null)
        
        if [ -n "$NEW_TOKEN" ] && [ "$NEW_TOKEN" != "null" ]; then
            print_info "System is now reachable! Boot completed successfully."
            print_info "New token retrieved: ${NEW_TOKEN:0:50}..."
            boot_successful=true
            TOKEN="$NEW_TOKEN"
            break
        fi
    fi
    
    # Sleep for 40 seconds before next attempt
    if [ $elapsed_time -lt $TIMEOUT ]; then
        sleep $SLEEP_INTERVAL
        elapsed_time=$((elapsed_time + SLEEP_INTERVAL))
    fi
done

echo ""

if [ "$boot_successful" = true ]; then
    print_info "============================================"
    print_info "Script completed successfully!"
    print_info "============================================"
    print_info "System booted and is now reachable."
    print_info "Total boot time: ${elapsed_time} seconds"
else
    print_error "============================================"
    print_error "Timeout reached!"
    print_error "============================================"
    print_error "System did not become reachable within 30 minutes."
    print_error "Please check the system status manually."
    exit 1
fi

echo ""

print_info "============================================"
print_info "Script completed successfully!"
print_info "============================================"
print_info "Token is stored in variable TOKEN and can be used for additional API calls."

# Export token for use in other scripts
export AUTH_TOKEN="$TOKEN"
print_info "Token exported as AUTH_TOKEN environment variable."
