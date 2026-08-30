# -*- coding: utf-8 -*-
# Copyright: (c) 2021, XLAB Steampunk <steampunk@xlab.si>
#
# GNU General Public License v3.0+ (see COPYING or https://www.gnu.org/licenses/gpl-3.0.txt)

from __future__ import absolute_import, division, print_function

__metaclass__ = type


class ServiceNowError(Exception):
    pass


class AuthError(ServiceNowError):
    pass


class UnexpectedAPIResponse(ServiceNowError):
    def __init__(self, status, data):
        self.message = "Unexpected response - {0} {1}".format(status, data)
        super(UnexpectedAPIResponse, self).__init__(self.message)
