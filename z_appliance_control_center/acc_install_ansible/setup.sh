#!/bin/bash
# *+------------------------------------------------------------------------+
# *| © Copyright IBM Corp. 2025                                             |
# *| [10.17.2025]                                                           |
# *|   - Tested with ACC 1.2.6                                              |
# *|   - Initial release                                                    |
# *| [12.12.2025]                                                           |
# *|   - Tested with ACC 1.2.10                                             |
# *+------------------------------------------------------------------------+

echo "Directory for ansible script"
echo $ANSIBLE_TMPDIR

# Step 1: Install sshpass
echo "Step 1: Checking for sshpass..."
if ! command -v sshpass >/dev/null 2>&1; then
  echo "Error: 'sshpass' is not installed. Please install it manually before running this script."
  echo "Refer to README.md for installation instructions."
  exit 1
else
  echo "'sshpass' is already installed."
fi

# Step 2: Check if the virtual environment already exists
if [ -d "venv" ]; then
  echo "Step 2: Virtual environment already exists."
else
  echo "Error: Virtual environment venv does not exist at $ANSIBLE_TMPDIR"
  echo "Refer to README.md for installation instructions."
  exit 1
fi

# Step 3: Activate the virtual environment
echo "Step 3: Activating virtual environment..."
source "$ANSIBLE_TMPDIR/venv/bin/activate"


# Step 4: Verify Python and pip availability
echo "Step 4: Checking Python and pip..."
if ! command -v python3 >/dev/null 2>&1; then
  echo "Error: 'python3' is not installed or not in PATH."
  echo "Please install Python 3 and retry."
  exit 1
fi

if ! command -v pip3 >/dev/null 2>&1; then
  echo "Error: 'pip3' is not installed or not in PATH."
  echo "Please install pip3 manually."
  exit 1
fi
echo "Python and pip are available."


# Step 5: Verify required Python packages
echo "Step 5: Verifying required Python packages..."
required_packages=("click" "click_shell" "zhmcclient" "urllib3")
missing_packages=()

for pkg in "${required_packages[@]}"; do
  python3 -c "import $pkg" 2>/dev/null
  if [ $? -ne 0 ]; then
    missing_packages+=("$pkg")
  fi
done

if [ ${#missing_packages[@]} -ne 0 ]; then
  echo "Error: Missing required Python packages:"
  for pkg in "${missing_packages[@]}"; do
    echo "  - $pkg"
  done
  echo "Please install them manually inside the virtual environment before running this script."
  echo "Example:"
  echo "  source $ANSIBLE_TMPDIR/venv/bin/activate"
  echo "  pip install ${missing_packages[*]}"
  echo "Refer to README.md for more details."
  deactivate
  exit 1
else
  echo "All required Python packages are available."
fi


# Step 6: Run the Python script (hmc_cli.py)
if [ -f "$ANSIBLE_TMPDIR/hmc_cli.py" ]; then
  echo "Step 5: Launching CLI..."
  python3 "$ANSIBLE_TMPDIR/hmc_cli.py"
else
  echo "$ANSIBLE_TMPDIR/hmc_cli.py not found!"
  exit 1
fi

