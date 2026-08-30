.. Copyright 2017-2020 IBM Corp. All Rights Reserved.
..
.. Licensed under the Apache License, Version 2.0 (the "License");
.. you may not use this file except in compliance with the License.
.. You may obtain a copy of the License at
..
..    http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS,
.. WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
.. See the License for the specific language governing permissions and
.. limitations under the License.
..


Managed node
============

The managed node for the **IBM Z HMC collection** is the targeted HMC. The
Z systems managed by the HMC are not visible on the network because the HMC
encapsulates them. In addition, there is a number of resources that are owned
by the HMC itself, such as users or password rules.

Requirements for the targeted HMC are:

* The HMC must have its Web Services API enabled.
