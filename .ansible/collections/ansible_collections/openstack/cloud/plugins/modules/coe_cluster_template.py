#!/usr/bin/python

# Copyright (c) 2018 Catalyst IT Ltd.
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

DOCUMENTATION = '''
---
module: coe_cluster_template
short_description: Add/Remove COE cluster template from OpenStack Cloud
author: OpenStack Ansible SIG
description:
   - Add or Remove COE cluster template from the OpenStack Container Infra
     service.
options:
   coe:
      description:
         - The Container Orchestration Engine for this clustertemplate
      choices: [kubernetes, swarm, mesos]
      type: str
      required: true
   dns_nameserver:
      description:
         - The DNS nameserver address
      default: '8.8.8.8'
      type: str
   docker_storage_driver:
      description:
         - Docker storage driver
      choices: [devicemapper, overlay, overlay2]
      type: str
   docker_volume_size:
      description:
         - The size in GB of the docker volume
      type: int
   external_network_id:
      description:
         - The external network to attach to the Cluster
      type: str
   fixed_network:
      description:
         - The fixed network name to attach to the Cluster
      type: str
   fixed_subnet:
      description:
         - The fixed subnet name to attach to the Cluster
      type: str
   flavor_id:
      description:
         - The flavor of the minion node for this ClusterTemplate
      type: str
   floating_ip_enabled:
      description:
         - Indicates whether created clusters should have a floating ip or not
      type: bool
      default: true
   keypair_id:
      description:
         - Name or ID of the keypair to use.
      type: str
   image_id:
      description:
         - Image id the cluster will be based on
      type: str
      required: true
   labels:
      description:
         - One or more key/value pairs
      type: raw
   http_proxy:
      description:
         - Address of a proxy that will receive all HTTP requests and relay them
           The format is a URL including a port number
      type: str
   https_proxy:
      description:
         - Address of a proxy that will receive all HTTPS requests and relay
           them. The format is a URL including a port number
      type: str
   master_flavor_id:
      description:
         - The flavor of the master node for this ClusterTemplate
      type: str
   master_lb_enabled:
      description:
         - Indicates whether created clusters should have a load balancer
           for master nodes or not
      type: bool
      default: 'no'
   name:
      description:
         - Name that has to be given to the cluster template
      required: true
      type: str
   network_driver:
      description:
         - The name of the driver used for instantiating container networks
      choices: [flannel, calico, docker]
      type: str
   no_proxy:
      description:
         - A comma separated list of IPs for which proxies should not be
           used in the cluster
      type: str
   public:
      description:
         - Indicates whether the ClusterTemplate is public or not
      type: bool
      default: 'no'
   registry_enabled:
      description:
         - Indicates whether the docker registry is enabled
      type: bool
      default: 'no'
   server_type:
      description:
         - Server type for this ClusterTemplate
      choices: [vm, bm]
      default: vm
      type: str
   state:
      description:
         - Indicate desired state of the resource.
      choices: [present, absent]
      default: present
      type: str
   tls_disabled:
      description:
         - Indicates whether the TLS should be disabled
      type: bool
      default: 'no'
   volume_driver:
      description:
         - The name of the driver used for instantiating container volumes
      choices: [cinder, rexray]
      type: str
requirements:
    - "python >= 3.6"
    - "openstacksdk"

extends_documentation_fragment:
- openstack.cloud.openstack
'''

RETURN = '''
id:
    description: The cluster UUID.
    returned: On success when I(state) is 'present'
    type: str
    sample: "39007a7e-ee4f-4d13-8283-b4da2e037c69"
cluster_template:
    description: Dictionary describing the template.
    returned: On success when I(state) is 'present'
    type: complex
    contains:
      coe:
          description: The Container Orchestration Engine for this clustertemplate
          type: str
          sample: kubernetes
      dns_nameserver:
          description: The DNS nameserver address
          type: str
          sample: '8.8.8.8'
      docker_storage_driver:
          description: Docker storage driver
          type: str
          sample: devicemapper
      docker_volume_size:
          description: The size in GB of the docker volume
          type: int
          sample: 5
      external_network_id:
          description: The external network to attach to the Cluster
          type: str
          sample: public
      fixed_network:
          description: The fixed network name to attach to the Cluster
          type: str
          sample: 07767ec6-85f5-44cb-bd63-242a8e7f0d9d
      fixed_subnet:
          description:
            - The fixed subnet name to attach to the Cluster
          type: str
          sample: 05567ec6-85f5-44cb-bd63-242a8e7f0d9d
      flavor_id:
          description:
            - The flavor of the minion node for this ClusterTemplate
          type: str
          sample: c1.c1r1
      floating_ip_enabled:
          description:
            - Indicates whether created clusters should have a floating ip or not
          type: bool
          sample: true
      keypair_id:
          description:
            - Name or ID of the keypair to use.
          type: str
          sample: mykey
      image_id:
          description:
            - Image id the cluster will be based on
          type: str
          sample: 05567ec6-85f5-44cb-bd63-242a8e7f0e9d
      labels:
          description: One or more key/value pairs
          type: dict
          sample: {'key1': 'value1', 'key2': 'value2'}
      http_proxy:
          description:
            - Address of a proxy that will receive all HTTP requests and relay them
              The format is a URL including a port number
          type: str
          sample: http://10.0.0.11:9090
      https_proxy:
          description:
            - Address of a proxy that will receive all HTTPS requests and relay
              them. The format is a URL including a port number
          type: str
          sample: https://10.0.0.10:8443
      master_flavor_id:
          description:
            - The flavor of the master node for this ClusterTemplate
          type: str
          sample: c1.c1r1
      master_lb_enabled:
          description:
            - Indicates whether created clusters should have a load balancer
              for master nodes or not
          type: bool
          sample: true
      name:
          description:
            - Name that has to be given to the cluster template
          type: str
          sample: k8scluster
      network_driver:
          description:
            - The name of the driver used for instantiating container networks
          type: str
          sample: calico
      no_proxy:
          description:
            - A comma separated list of IPs for which proxies should not be
              used in the cluster
          type: str
          sample: 10.0.0.4,10.0.0.5
      public:
          description:
            - Indicates whether the ClusterTemplate is public or not
          type: bool
          sample: false
      registry_enabled:
          description:
            - Indicates whether the docker registry is enabled
          type: bool
          sample: false
      server_type:
          description:
            - Server type for this ClusterTemplate
          type: str
          sample: vm
      tls_disabled:
          description:
            - Indicates whether the TLS should be disabled
          type: bool
          sample: false
      volume_driver:
          description:
            - The name of the driver used for instantiating container volumes
          type: str
          sample: cinder
'''

EXAMPLES = '''
# Create a new Kubernetes cluster template
- openstack.cloud.coe_cluster_template:
    name: k8s
    coe: kubernetes
    keypair_id: mykey
    image_id: 2a8c9888-9054-4b06-a1ca-2bb61f9adb72
    public: no
'''

from ansible_collections.openstack.cloud.plugins.module_utils.openstack import OpenStackModule


class CoeClusterTemplateModule(OpenStackModule):
    argument_spec = dict(
        coe=dict(required=True, choices=['kubernetes', 'swarm', 'mesos']),
        dns_nameserver=dict(default='8.8.8.8'),
        docker_storage_driver=dict(choices=['devicemapper', 'overlay', 'overlay2']),
        docker_volume_size=dict(type='int'),
        external_network_id=dict(default=None),
        fixed_network=dict(default=None),
        fixed_subnet=dict(default=None),
        flavor_id=dict(default=None),
        floating_ip_enabled=dict(type='bool', default=True),
        keypair_id=dict(default=None),
        image_id=dict(required=True),
        labels=dict(default=None, type='raw'),
        http_proxy=dict(default=None),
        https_proxy=dict(default=None),
        master_lb_enabled=dict(type='bool', default=False),
        master_flavor_id=dict(default=None),
        name=dict(required=True),
        network_driver=dict(choices=['flannel', 'calico', 'docker']),
        no_proxy=dict(default=None),
        public=dict(type='bool', default=False),
        registry_enabled=dict(type='bool', default=False),
        server_type=dict(default="vm", choices=['vm', 'bm']),
        state=dict(default='present', choices=['absent', 'present']),
        tls_disabled=dict(type='bool', default=False),
        volume_driver=dict(choices=['cinder', 'rexray']),
    )
    module_kwargs = dict()

    def _parse_labels(self, labels):
        if isinstance(labels, str):
            labels_dict = {}
            for kv_str in labels.split(","):
                k, v = kv_str.split("=")
                labels_dict[k] = v
            return labels_dict
        if not labels:
            return {}
        return labels

    def run(self):
        params = self.params.copy()

        state = self.params['state']
        name = self.params['name']
        coe = self.params['coe']
        image_id = self.params['image_id']

        kwargs = dict(
            dns_nameserver=self.params['dns_nameserver'],
            docker_storage_driver=self.params['docker_storage_driver'],
            docker_volume_size=self.params['docker_volume_size'],
            external_network_id=self.params['external_network_id'],
            fixed_network=self.params['fixed_network'],
            fixed_subnet=self.params['fixed_subnet'],
            flavor_id=self.params['flavor_id'],
            floating_ip_enabled=self.params['floating_ip_enabled'],
            keypair_id=self.params['keypair_id'],
            labels=self._parse_labels(params['labels']),
            http_proxy=self.params['http_proxy'],
            https_proxy=self.params['https_proxy'],
            master_lb_enabled=self.params['master_lb_enabled'],
            master_flavor_id=self.params['master_flavor_id'],
            network_driver=self.params['network_driver'],
            no_proxy=self.params['no_proxy'],
            public=self.params['public'],
            registry_enabled=self.params['registry_enabled'],
            server_type=self.params['server_type'],
            tls_disabled=self.params['tls_disabled'],
            volume_driver=self.params['volume_driver'],
        )

        changed = False
        template = self.conn.get_coe_cluster_template(
            name_or_id=name, filters={'coe': coe, 'image_id': image_id})

        if state == 'present':
            if not template:
                template = self.conn.create_coe_cluster_template(
                    name, coe=coe, image_id=image_id, **kwargs)
                changed = True
            else:
                changed = False

            self.exit_json(
                changed=changed, cluster_template=template, id=template['uuid'])
        elif state == 'absent':
            if not template:
                self.exit_json(changed=False)
            else:
                self.conn.delete_coe_cluster_template(name)
                self.exit_json(changed=True)


def main():
    module = CoeClusterTemplateModule()
    module()


if __name__ == "__main__":
    main()
