# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

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