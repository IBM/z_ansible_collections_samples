# Developing collection

In this guide, we will go through the process of setting up a development
environment.


### Prerequisites

There are a few things we will need to have installed on our workstation. They
are, in no particular order:

  * a Python 3 interpreter with virtual environment support,
  * git, and
  * a functioning Docker installation.

It should be relatively straightforward to get those things installed on our
machine. Once we have them ready, we can start customizing our environment for
Ansible development.


### Obtaining the source code

ServiceNow Ansible Collection lives on GitHub, which means that we are just
one command away from the full source code. But before we can clone the
repository, we need to create a few directories that will hold the source
code:

    $ mkdir -p ansible_collections/servicenow
    $ cd ansible_collections/servicenow
    $ git clone git@github.com:ansible-collections/servicenow.itsm.git itsm
    $ cd itsm

It is vitally important that we:

 1. create two parent directories before checking out the code, and
 2. clone the code into the `itsm` directory.

Ansible development tools often assume that we are working from the
`ansible_collections/<namespace>/<collection>` directory and complain loudly
if they do not have it their way.

And this is it.


### Installing development tools

The first thing we need to do is create a new virtual environment and activate
it.

    $ python3 -m venv ../../../venv
    $ . ../../../venv/bin/activate

We intentionally created our virtual environment outside the collection
directory. This is a simple way of making sure that various static analysis
tools only report issues with our code and not with any of the dependencies.

Now we need to install `ansible-base`. Since we are just starting, latest
stable version will do just fine:

    (venv) $ pip install ansible-base

There is just one more thing left for us to do: test the setup.


### Testing the setup

To validate our setup, we can run the bundled tests and render the
documentation:

    (venv) $ make sanity
    (venv) $ make units

And if all of the commands finished with no errors, we are ready to start
developing.
