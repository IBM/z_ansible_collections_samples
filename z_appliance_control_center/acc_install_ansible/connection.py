#*+-------------------------------------------------------------------+
#*| # © Copyright IBM Corp. 2025                                      |
#*| # This playbook is tested with ACC 1.2.6                          |
#*+-------------------------------------------------------------------+

import subprocess
import click
import os

# from config import CONFIG

def is_tunnel_active():
    """Checks if the SSH tunnel is already running."""
    try:
        result = subprocess.run(
            "pgrep -f 'ssh -o StrictHostKeyChecking=no'", 
            shell=True, capture_output=True, text=True
        )
        return bool(result.stdout.strip())  # If output is not empty, SSH is running
    except subprocess.SubprocessError as e:
        click.echo(f"Error checking tunnel status: {e}")
        return False

def start_tunneling():
    """Starts SSH tunneling using values from config.py if not already running."""
    
    if is_tunnel_active():
        click.echo("SSH tunnel is already active, please check 'ps -ef | grep ssh'")
        return
    
    local_server_port = os.environ.get("LOCAL_SERVER_PORT")
    local_ssh_port = os.environ.get("LOCAL_SSH_PORT")
    gateway_ip = os.environ.get("GATEWAY_IP")
    gateway_user = os.environ.get("GATEWAY_USER")
    https_port = os.environ.get("HTTPS_PORT")
    ssh_port = os.environ.get("SSH_PORT")
    d_port = os.environ.get("D_PORT")
    lpar_ip = os.environ.get("LPAR_IP")
    ssh_key = os.environ.get("HMC_KEY")

    # SSH command for dynamic port forwarding
    ssh_cmd_1 = f"ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -i {ssh_key} -D {d_port} -f -C -q -N {gateway_user}@{gateway_ip}"

    # SSH command for local port forwarding
    ssh_cmd_2 = f"ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -i {ssh_key} -L {local_server_port}:{lpar_ip}:{https_port} -f -C -q -N {gateway_user}@{gateway_ip}"

    # SSH command for local port forwarding
    ssh_cmd_3 = f"ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -i {ssh_key} -L {local_ssh_port}:{lpar_ip}:{ssh_port} -f -C -q -N {gateway_user}@{gateway_ip}"

    try:
        click.echo("Starting SSH tunnel...")
        subprocess.run(ssh_cmd_1, shell=True, check=True)
        subprocess.run(ssh_cmd_2, shell=True, check=True)
        subprocess.run(ssh_cmd_3, shell=True, check=True)
        click.echo("SSH tunnel started successfully.")
    except subprocess.CalledProcessError as e:
        click.echo(f"Error starting tunnel: {e}")

def stop_tunneling():
    """Stops SSH tunneling by killing SSH processes."""
    if not is_tunnel_active():
        click.echo("No active SSH tunnels found.")
        return

    try:
        subprocess.run("pkill -f 'ssh -o StrictHostKeyChecking=no'", shell=True, check=True)
        click.echo("SSH tunnels stopped successfully.")
    except subprocess.CalledProcessError as e:
        click.echo(f"Failed to stop SSH tunnels: {e}")
