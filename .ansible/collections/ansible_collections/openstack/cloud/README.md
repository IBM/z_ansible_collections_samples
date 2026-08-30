[![OpenDev Zuul Builds - Ansible Collection OpenStack](https://zuul-ci.org/gated.svg)](http://zuul.opendev.org/t/openstack/builds?project=openstack%2Fansible-collections-openstack#)

# Ansible Collection: openstack.cloud

This repo hosts the `openstack.cloud` Ansible Collection.

The collection includes the Openstack modules and plugins supported by Openstack community to help the management of Openstack infrastructure.

## Breaking backward compatibility :warning:

Dear contributors and users of the Ansible OpenStack collection!
Our codebase has been split into two separate release series:

* `2.x.x` releases of Ansible OpenStack collection are compatible with OpenStack SDK `1.x.x` and its release candidates
  `0.99.x` *only* (OpenStack Zed and later). Our `master` branch tracks our `2.x.x` releases.
* `1.x.x` releases of Ansible OpenStack collection are compatible with OpenStack SDK `0.x.x` prior to `0.99.0` *only*
  (OpenStack Yoga and earlier). Our `stable/1.0.0` branch tracks our `1.x.x` releases.

Both branches will be developed in parallel for the time being. Patches from `master` will be backported to
`stable/1.0.0` on a best effort basis but expect new features to be introduced in our `master` branch only.
Contributions are welcome for both branches!
Differences between both branches are mainly renamed and sometimes dropped module return values. We try to keep our
module parameters backward compatible by offering aliases but e.g. the semantics of `filters` parameters in `*_info`
modules have changed due to updates in the OpenStack SDK.

Our decision to break backward compatibility was not taken lightly. OpenStack SDK's first major release (`1.0.0` and its
release candidates `0.99.x`) has streamlined and improved large parts of its codebase. For example, its Connection
interface now consistently uses the Resource interfaces under the hood. This required breaking changes from older SDK
releases though. The Ansible OpenStack collection is heavily based on OpenStack SDK. With OpenStack SDK becoming
backward incompatible, so does our Ansible OpenStack collection. We simply lack the devpower to maintain a backward
compatible interface in Ansible OpenStack collection across several SDK releases.

Our first `2.0.0` release is currently under development and we still have a long way to go. If you use modules of the
Ansible OpenStack collection and want to join us in porting them to the upcoming OpenStack SDK, please contact us!
Ping Jakob Meng <mail@jakobmeng.de> (jm1) or Rafael Castillo <rcastill@redhat.com> (rcastillo) and we will give you a
quick introduction. We are also hanging around on `irc.oftc.net/#openstack-ansible-sig` and `irc.oftc.net/#oooq` 😎

We have extensive documentation on [why, what and how we are adopting and reviewing the new modules](
https://hackmd.io/szgyWa5qSUOWw3JJBXLmOQ?view), [how to set up a working DevStack environment for hacking on the
collection](https://hackmd.io/PI10x-iCTBuO09duvpeWgQ?view) and, most importantly, [a list of modules where we are
coordinating our porting efforts](https://hackmd.io/7NtovjRkRn-tKraBXfz9jw?view).

## Installation and Usage

### Installing dependencies

For using the Openstack Cloud collection firstly you need to install `ansible` and `openstacksdk` Python modules on your Ansible controller.
For example with pip:

```bash
pip install "ansible>=2.9" "openstacksdk>=0.36,<0.99.0"
```

OpenStackSDK has to be available to Ansible and to the Python interpreter on the host, where Ansible executes the module (target host).
Please note, that under some circumstances Ansible might invoke a non-standard Python interpreter on the target host.
Using Python version 3 is highly recommended for OpenstackSDK and strongly required from OpenstackSDK version 0.39.0.

---

#### NOTE

OpenstackSDK is better to be the last stable version. It should NOT be installed on Openstack nodes,
but rather on operators host (aka "Ansible controller"). OpenstackSDK from last version supports
operations on all Openstack cloud versions. Therefore OpenstackSDK module version doesn't have to match
Openstack cloud version usually.

---

### Installing the Collection from Ansible Galaxy

Before using the Openstack Cloud collection, you need to install the collection with the `ansible-galaxy` CLI:

`ansible-galaxy collection install openstack.cloud`

You can also include it in a `requirements.yml` file and install it through `ansible-galaxy collection install -r requirements.yml` using the format:

```yaml
collections:
- name: openstack.cloud
```

### Playbooks

To use a module from the Openstack Cloud collection, please reference the full namespace, collection name, and module name that you want to use:

```yaml
---
- name: Using Openstack Cloud collection
  hosts: localhost
  tasks:
    - openstack.cloud.server:
        name: vm
        state: present
        cloud: openstack
        region_name: ams01
        image: Ubuntu Server 14.04
        flavor_ram: 4096
        boot_from_volume: True
        volume_size: 75
```

Or you can add the full namespace and collection name in the `collections` element:

```yaml
---
- name: Using Openstack Cloud collection
  hosts: localhost
  collections:
    - openstack.cloud
  tasks:
    - server_volume:
        state: present
        cloud: openstack
        server: Mysql-server
        volume: mysql-data
        device: /dev/vdb
```

### Usage

See the collection docs at Ansible site:

* [openstack.cloud collection docs (version released in Ansible package)](https://docs.ansible.com/ansible/latest/collections/openstack/cloud/index.html)

* [openstack.cloud collection docs (devel version)](https://docs.ansible.com/ansible/devel/collections/openstack/cloud/index.html)

## Contributing

For information on contributing, please see [CONTRIBUTING](https://opendev.org/openstack/ansible-collections-openstack/src/branch/master/CONTRIBUTING.rst)

There are many ways in which you can participate in the project, for example:

- Submit [bugs and feature requests](https://storyboard.openstack.org/#!/project/openstack/ansible-collections-openstack), and help us verify them
- Submit and review source code changes in [Openstack Gerrit](https://review.opendev.org/#/q/project:openstack/ansible-collections-openstack)
- Add new modules for Openstack Cloud

We work with [OpenDev Gerrit](https://review.opendev.org/), pull requests submitted through GitHub will be ignored.

## Testing and Development

If you want to develop new content for this collection or improve what is already here, the easiest way to work on the collection is to clone it into one of the configured [`COLLECTIONS_PATHS`](https://docs.ansible.com/ansible/latest/reference_appendices/config.html#collections-paths), and work on it there.

### Testing with `ansible-test`

We use `ansible-test` for sanity:

```bash
tox -e linters
```

## More Information

TBD

## Communication

We have a dedicated Interest Group for Openstack Ansible modules.
You can find other people interested in this in `#openstack-ansible-sig` on [OFTC IRC](https://www.oftc.net/).

## License

GNU General Public License v3.0 or later

See [LICENCE](https://opendev.org/openstack/ansible-collections-openstack/src/branch/master/COPYING) to see the full text.
