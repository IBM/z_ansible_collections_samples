#!/bin/bash
set -e

# Variable Setup
source $HOME/.bashrc

LOCAL_REGISTRY_USERNAME="icic"
LOCAL_REGISTRY_PASSWORD="icic"
LOCAL_REGISTRY_HOSTNAME="image.registry.icic.ocp.com"
LOCAL_REGISTRY_PORT="5008"

GENERATE_CRT="true" #"true" to generate self-sign certificates

# Persist Answers
grep -q LOCAL_REGISTRY_USERNAME $HOME/.bashrc || echo "export LOCAL_REGISTRY_USERNAME=$LOCAL_REGISTRY_USERNAME" >> $HOME/.bashrc
grep -q LOCAL_REGISTRY_PASSWORD $HOME/.bashrc || echo "export LOCAL_REGISTRY_PASSWORD=$LOCAL_REGISTRY_PASSWORD" >> $HOME/.bashrc
grep -q LOCAL_REGISTRY_HOSTNAME $HOME/.bashrc || echo "export LOCAL_REGISTRY_HOSTNAME=$LOCAL_REGISTRY_HOSTNAME" >> $HOME/.bashrc
grep -q LOCAL_REGISTRY_PORT $HOME/.bashrc || echo "export LOCAL_REGISTRY_PORT=$LOCAL_REGISTRY_PORT" >> $HOME/.bashrc

echo "Preparing required packages..."
yum -y install podman httpd-tools wget jq firewalld -q
mkdir -p /opt/registry/{auth,certs,data}
systemctl enable firewalld
systemctl start firewalld

# Self-sign certificate
if [ "${GENERATE_CRT}" == "true" ];then
  echo "Generating self-sign certificates..."
  cd /opt/registry/certs
  rm -rf domain.crt
  openssl req -newkey rsa:4096 -nodes -sha256 -keyout domain.key -x509 -days 365 -out domain.crt -subj "/CN=${LOCAL_REGISTRY_HOSTNAME}" -addext "subjectAltName=DNS:${LOCAL_REGISTRY_HOSTNAME}">/dev/null 2>&1
  cp /opt/registry/certs/domain.crt /etc/pki/ca-trust/source/anchors/
  update-ca-trust
fi

# Create Htpasswd
echo "Creating htpasswd..."
htpasswd -bBc /opt/registry/auth/htpasswd "${LOCAL_REGISTRY_USERNAME}" "${LOCAL_REGISTRY_PASSWORD}" >/dev/null

# Setup Firewall Rules
echo "Modifying firewall rules..."
firewall-cmd --zone=public --remove-port=${LOCAL_REGISTRY_PORT}/tcp
firewall-cmd --zone=internal --remove-port=${LOCAL_REGISTRY_PORT}/tcp
firewall-cmd --runtime-to-permanent
firewall-cmd --reload
firewall-cmd --add-port=${LOCAL_REGISTRY_PORT}/tcp --zone=internal --permanent >/dev/null
firewall-cmd --add-port=${LOCAL_REGISTRY_PORT}/tcp --zone=public   --permanent >/dev/null
firewall-cmd --reload >/dev/null

echo "Deleting registry container..."
exist=$(podman ps -a | grep ocp-registry | wc -l)
if [ "${exist}" == "1" ];then
  podman rm -f ocp-registry
fi
# Create Registry Container
echo "Creating registry container..."
podman create --name ocp-registry -p ${LOCAL_REGISTRY_PORT}:5000 \
-v /opt/registry/data:/var/lib/registry:z \
-v /opt/registry/auth:/auth:z \
-e "REGISTRY_AUTH=htpasswd" \
-e "REGISTRY_AUTH_HTPASSWD_REALM=Registry" \
-e "REGISTRY_HTTP_SECRET=ALongRandomSecretForRegistry" \
-e REGISTRY_AUTH_HTPASSWD_PATH=/auth/htpasswd \
-v /opt/registry/certs:/certs:z \
-e REGISTRY_HTTP_TLS_CERTIFICATE=/certs/domain.crt \
-e REGISTRY_HTTP_TLS_KEY=/certs/domain.key \
docker.io/ibmcom/registry:2.6.2.5 >/dev/null

echo "Deleting registry service..."
rm -rf /etc/systemd/system/ocp-registry.service
echo "Enabling registry service..."
echo '[Unit]
Description=ocp-registry Podman Container
After=network.target

[Service]
Type=simple
Restart=always
ExecStart=/usr/bin/podman start -a ocp-registry
ExecStop=/usr/bin/podman stop -t 10 ocp-registry

[Install]
WantedBy=multi-user.target' > /etc/systemd/system/ocp-registry.service

systemctl daemon-reload
systemctl start ocp-registry
systemctl enable ocp-registry >/dev/null 2>&1

host_ip_addr=$(hostname -I | cut -d' ' -f1)
echo "Add registry into /etc/hosts..."
echo "${host_ip_addr} ${LOCAL_REGISTRY_HOSTNAME}" >> /etc/hosts

sleep 5

# Test
if [[ $(curl -s -o /dev/null -w "%{http_code}" -u "${LOCAL_REGISTRY_USERNAME}":"${LOCAL_REGISTRY_PASSWORD}" -k "https://${LOCAL_REGISTRY_HOSTNAME}:${LOCAL_REGISTRY_PORT}/v2/_catalog") != '200' ]];then
  echo "Cannot connect to registry" && exit 1
fi

echo "Registry installed successfully"