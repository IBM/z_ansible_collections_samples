#!/bin/bash
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

echo "Directory for ansible script"
echo $ANSIBLE_TMPDIR

# Detect OS
ARCH=$(uname -m)
KERNEL=$(uname -s)
OS_ID="unknown"

if [ "$KERNEL" = "Darwin" ]; then
  # macOS
  OS_ID="darwin"
else
  if [ -r /etc/os-release ]; then
    . /etc/os-release
    if [ -n "${ID:-}" ]; then
      OS_ID=$ID
    elif [ -n "${NAME:-}" ]; then
      # fallback: use NAME if ID not present
      OS_ID=$(echo "$NAME" | tr '[:upper:]' '[:lower:]')
    fi
  fi

  # extra fallback if still unknown
  if [ "$OS_ID" = "unknown" ]; then
    if [ -r /etc/debian_version ]; then
      OS_ID="debian"
    elif [ -r /etc/redhat-release ]; then
      OS_ID="rhel"
    elif command -v lsb_release >/dev/null 2>&1; then
      OS_ID=$(lsb_release -is | tr '[:upper:]' '[:lower:]')
    fi
  fi
fi

echo "Detected OS : $OS_ID"
echo "Detected Architecture : $ARCH"



# Step 1: Install sshpass
echo "Step 1: Installing sshpass..."
case "$OS_ID" in
  ubuntu|debian)
    if [ "$ARCH" = "s390x" ]; then
      echo "s390x detected → building sshpass from source..."
      sudo apt update
      sudo apt install -y build-essential wget
      wget http://downloads.sourceforge.net/project/sshpass/sshpass/1.09/sshpass-1.09.tar.gz
      tar -xvzf sshpass-1.09.tar.gz
      cd sshpass-1.09
      ./configure
      make
      sudo make install
      cd ..
    else
      echo "Using apt to install sshpass..."
      sudo apt-get update
      sudo apt-get install -y sshpass
    fi
    ;;
  rhel|centos|fedora)
    echo "Using yum to install sshpass..."
    sudo yum install -y sshpass
    ;;
  darwin)
    echo "Using Homebrew on macOS to install sshpass..."
    if command -v brew >/dev/null 2>&1; then
      brew install hudochenkov/sshpass/sshpass
    else
      echo "Homebrew is not installed. Please install Homebrew first: https://brew.sh/"
      exit 1
    fi
    ;;
  brew)
    echo "Using Homebrew to install sshpass..."
    brew install sshpass
    ;;
    *)
    if command -v brew >/dev/null 2>&1; then
      echo "Unknown OS, but Homebrew is available. Using brew..."
      brew install sshpass
    else
      echo "Unsupported OS: $OS_ID and no known package manager found."
      exit 1
    fi
    ;;
esac


# Step 2: Check if the virtual environment already exists
if [ -d "venv" ]; then
  echo "Step 2: Virtual environment already exists. Activating it..."
else
  echo "Step 2: Creating virtual environment..."
  python3 -m venv "$ANSIBLE_TMPDIR/venv"
fi

# Step 3: Activate the virtual environment
echo "Step 3: Activating virtual environment..."
source "$ANSIBLE_TMPDIR/venv/bin/activate"

# Step 4: Install Python dependencies from requirements.txt

echo $ANSIBLE_TMPDIR/requirements.txt

if [ -f "$ANSIBLE_TMPDIR/requirements.txt" ]; then
  echo "Step 4: Installing dependencies..."
  pip3 install -r "$ANSIBLE_TMPDIR/requirements.txt"
else
  echo "$ANSIBLE_TMPDIR/requirements.txt not found!"
  exit 1
fi

# Step 5: Set environment variables for the Python script
echo "Step 5: Setting environment variables..."

# Step 5: Run the Python script (hmc_cli.py)
if [ -f "$ANSIBLE_TMPDIR/hmc_cli.py" ]; then
  echo "Step 5: Launching CLI..."
  python3 "$ANSIBLE_TMPDIR/hmc_cli.py"
else
  echo "$ANSIBLE_TMPDIR/hmc_cli.py not found!"
  exit 1
fi

