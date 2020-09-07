###############################################################################
# © Copyright IBM Corporation 2020
# Contributed and supported by the Ansible Content for IBM Z Team
#
# Changelog
#  All notable changes to this sample will be documented in this playbook
#
# [1.0.0] - 2020-09-01
#  - Released initial version
###############################################################################

###############################################################################
# Requirements:
#     - IBM z/OS core collection 1.0.0 or later
###############################################################################

from __future__ import absolute_import, division, print_function

__metaclass__ = type


def zoau_version_command_string(potential_paths):
    potential_zoau_paths = potential_paths.get("zoau", [])
    command = "export _BPXK_AUTOCVT=ON ; export LIBPATH=/lib:/usr/lib:. ; {0}".format(
        " ; ".join(
            [
                "export PATH=/bin:{0}/bin ; {0}/bin/zoaversion".format(zoau.get("root"))
                for zoau in potential_zoau_paths
            ]
        )
    )
    return command


class FilterModule(object):
    """ Jinja2 filters for building raw commands. """

    def filters(self):
        filters = {
            "zoau_version_command_string": zoau_version_command_string,
        }
        return filters