#!/bin/bash
set -e

# Variable Setup
source $HOME/.bashrc

LOCAL_REGISTRY_USERNAME="redhat"
LOCAL_REGISTRY_PASSWORD="redhat"
LOCAL_REGISTRY_HOSTNAME="image.registry.ats.ocp410.com"
LOCAL_REGISTRY_PORT="5008"

GENERATE_CRT="true" #"true" to generate self-sign certificates

if [ -z "${LOCAL_REGISTRY_USERNAME}" ];then
  read -r -p "Enter registry username [redhat]: " input
  LOCAL_REGISTRY_USERNAME=${input:-redhat}
fi

if [ -z "${LOCAL_REGISTRY_PASSWORD}" ];then
  read -r -p "Enter registry password [redhat]: " input
  LOCAL_REGISTRY_PASSWORD=${input:-redhat}
fi

if [ -z "${LOCAL_REGISTRY_HOSTNAME}" ];then
  read -r -p "Enter registry URL [$(hostname -f)]: " input
  LOCAL_REGISTRY_HOSTNAME=${input:-$(hostname -f)}
fi

if [ -z "${LOCAL_REGISTRY_PORT}" ];then
  read -r -p "Enter registry port [5008]: " input
  LOCAL_REGISTRY_PORT=${input:-5008}
fi

if [ -z "${GENERATE_CRT}" ];then
  read -r -p "Generate self sign certificate? (Only will generate if true) [true]: " input
  GENERATE_CRT=${input:-true}
fi

# Persist Answers
grep -q LOCAL_REGISTRY_USERNAME $HOME/.bashrc || echo "export LOCAL_REGISTRY_USERNAME=$LOCAL_REGISTRY_USERNAME" >> $HOME/.bashrc
grep -q LOCAL_REGISTRY_PASSWORD $HOME/.bashrc || echo "export LOCAL_REGISTRY_PASSWORD=$LOCAL_REGISTRY_PASSWORD" >> $HOME/.bashrc
grep -q LOCAL_REGISTRY_HOSTNAME $HOME/.bashrc || echo "export LOCAL_REGISTRY_HOSTNAME=$LOCAL_REGISTRY_HOSTNAME" >> $HOME/.bashrc
grep -q LOCAL_REGISTRY_PORT $HOME/.bashrc || echo "export LOCAL_REGISTRY_PORT=$LOCAL_REGISTRY_PORT" >> $HOME/.bashrc

echo "Preparing required packages..."
yum -y install podman httpd-tools wget jq -q
mkdir -p /opt/registry/{auth,certs,data}

# Self-sign certificate
if [ "${GENERATE_CRT}" == "true" ];then
  echo "Generating self-sign certificates..."
  cd /opt/registry/certs
  openssl req -newkey rsa:4096 -nodes -sha256 -keyout domain.key -x509 -days 365 -out domain.crt -subj "/CN=${LOCAL_REGISTRY_HOSTNAME}" -addext "subjectAltName=DNS:${LOCAL_REGISTRY_HOSTNAME}">/dev/null 2>&1
  cp /opt/registry/certs/domain.crt /etc/pki/ca-trust/source/anchors/
  update-ca-trust
fi

# Create Htpasswd
echo "Creating htpasswd..."
htpasswd -bBc /opt/registry/auth/htpasswd "${LOCAL_REGISTRY_USERNAME}" "${LOCAL_REGISTRY_PASSWORD}" >/dev/null

# Setup Firewall Rules
echo "Modifying firewall rules..."
firewall-cmd --add-port=${LOCAL_REGISTRY_PORT}/tcp --zone=internal --permanent >/dev/null
firewall-cmd --add-port=${LOCAL_REGISTRY_PORT}/tcp --zone=public   --permanent >/dev/null
firewall-cmd --reload >/dev/null

# Create Registry Container
echo "Creating registry container..."
podman create --name ocp410-registry -p ${LOCAL_REGISTRY_PORT}:5000 \
-v /opt/registry/data:/var/lib/registry:z \
-v /opt/registry/auth:/auth:z \
-e "REGISTRY_AUTH=htpasswd" \
-e "REGISTRY_AUTH_HTPASSWD_REALM=Registry" \
-e "REGISTRY_HTTP_SECRET=ALongRandomSecretForRegistry" \
-e REGISTRY_AUTH_HTPASSWD_PATH=/auth/htpasswd \
-v /opt/registry/certs:/certs:z \
-e REGISTRY_HTTP_TLS_CERTIFICATE=/certs/domain.crt \
-e REGISTRY_HTTP_TLS_KEY=/certs/domain.key \
localhost/gcp0571/docker_registry2:latest >/dev/null

echo "Enabling registry service..."
echo '[Unit]
Description=ocp410-registry Podman Container
After=network.target

[Service]
Type=simple
Restart=always
ExecStart=/usr/bin/podman start -a ocp410-registry
ExecStop=/usr/bin/podman stop -t 10 ocp410-registry

[Install]
WantedBy=multi-user.target' > /etc/systemd/system/ocp410-registry.service

systemctl start ocp410-registry
systemctl enable ocp410-registry >/dev/null 2>&1

sleep 5

# Test
if [[ $(curl -s -o /dev/null -w "%{http_code}" -u "${LOCAL_REGISTRY_USERNAME}":"${LOCAL_REGISTRY_PASSWORD}" -k "https://${LOCAL_REGISTRY_HOSTNAME}:${LOCAL_REGISTRY_PORT}/v2/_catalog") != '200' ]];then
  echo "Cannot connect to registry" && exit 1
fi

echo "Registry installed successfully"