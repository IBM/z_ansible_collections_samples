#!/bin/bash
set -e

# Variable Setup
source $HOME/.bashrc

LOCAL_REGISTRY_USERNAME="icic"
LOCAL_REGISTRY_PASSWORD="icic"
LOCAL_REGISTRY_HOSTNAME="image.registry.icic.ocp.com"
LOCAL_REGISTRY_PORT="5008"
VERSION="4.8.14"
PULL_SECRET=''

CLIENT_ARCH="s390x"
IMAGE_ARCH="s390x"

if [ -z "$PULL_SECRET" ]
then
      echo "PULL_SECRET is empty" && exit 1
fi

# Adding registry to your pull-secret
echo "*** Adding registry to your pull-secret ****"
TOKEN=`echo -n $LOCAL_REGISTRY_USERNAME:$LOCAL_REGISTRY_PASSWORD  | base64 -w0`
HOSTPORT=${LOCAL_REGISTRY_HOSTNAME}:${LOCAL_REGISTRY_PORT}
REGISTRY_EMAIL="registry@${LOCAL_REGISTRY_HOSTNAME}"

rm -rf ./pull-secret
rm -rf ./pull-secret-dup.json
rm -rf pull-secret.json pull-secret-dup.json 
rm -rf mirror-registry.log

echo ${PULL_SECRET} >> ./pull-secret

# Make a copy of your pull secret in JSON format:
cat ./pull-secret | jq .  > ./pull-secret-dup.json

# Add your credentials to  pull-secret.json
cat pull-secret-dup.json | jq '.auths |= . +  {"HOSTPORT": { "auth": "TOKEN", "email": "you@example.com"}}' pull-secret-dup.json > pull-secret.json

sed -i "s/TOKEN/$TOKEN/g" pull-secret.json
sed -i "s/HOSTPORT/${HOSTPORT}/g" pull-secret.json
sed -i "s/you@example.com/$REGISTRY_EMAIL/g" pull-secret.json

BUILDNAME="ocp"
BUILDNUMBER="$(wget -q -O - "https://mirror.openshift.com/pub/openshift-v4/${IMAGE_ARCH}/clients/${BUILDNAME}/${VERSION}/release.txt" | grep 'Name:' | awk '{print $NF}')"

if [ "$(echo "${BUILDNUMBER}" | cut -d '.' -f1-2)" == "4.2" ] && [ "$(echo "${BUILDNUMBER}" | cut -d '.' -f3)" -lt "14" ];then
  OCP_RELEASE="${BUILDNUMBER}"
else
  OCP_RELEASE="${BUILDNUMBER}-${IMAGE_ARCH}"
fi

LOCAL_REGISTRY="${LOCAL_REGISTRY_HOSTNAME}:${LOCAL_REGISTRY_PORT}"
LOCAL_REPOSITORY='icic/openshift4'
PRODUCT_REPO="$(wget -q -O - "https://mirror.openshift.com/pub/openshift-v4/${IMAGE_ARCH}/clients/${BUILDNAME}/${VERSION}/release.txt" | grep "Pull From:" | cut -d '/' -f2)"
LOCAL_SECRET_JSON="./pull-secret.json"
RELEASE_NAME="$(wget -q -O - "https://mirror.openshift.com/pub/openshift-v4/${IMAGE_ARCH}/clients/${BUILDNAME}/${VERSION}/release.txt" | grep "Pull From:" | cut -d '/' -f3 | cut -d '@' -f1)"

# Download openshift client
wget https://mirror.openshift.com/pub/openshift-v4/${CLIENT_ARCH}/clients/ocp/${VERSION}/openshift-client-linux.tar.gz
tar -zvxf openshift-client-linux.tar.gz
rm -rf openshift-client-linux.tar.gz
# Mirroring Images
echo "Mirroring Images..."
echo "GODEBUG=x509ignoreCN=0 ./oc adm release mirror -a "${LOCAL_SECRET_JSON}" --from="quay.io/${PRODUCT_REPO}/${RELEASE_NAME}:${OCP_RELEASE}" --to-release-image="${LOCAL_REGISTRY}/${LOCAL_REPOSITORY}:${OCP_RELEASE}" --to="${LOCAL_REGISTRY}/${LOCAL_REPOSITORY}""

GODEBUG=x509ignoreCN=0 ./oc adm release mirror -a "${LOCAL_SECRET_JSON}" \
--from="quay.io/${PRODUCT_REPO}/${RELEASE_NAME}:${OCP_RELEASE}" \
--to-release-image="${LOCAL_REGISTRY}/${LOCAL_REPOSITORY}:${OCP_RELEASE}" \
--to="${LOCAL_REGISTRY}/${LOCAL_REPOSITORY}"