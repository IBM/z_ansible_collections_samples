# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import absolute_import, division, print_function

__metaclass__ = type

from string import ascii_lowercase, ascii_uppercase, digits
from random import choice
from re import match, MULTILINE

def generate_password(input, pass_len=8):
    """Generate a password.

    Returns:
        str: The generated password.
    """
    choices = [
        ascii_uppercase,
        digits,
    ]
    password = "".join([choice(choice(choices)) for x in range(pass_len)])
    return password


def generate_passphrase(input, pass_len=16, userid=None):
    """Generate a passphrase.

    Returns:
        str: The generated passphrase.
    """
    choices = [
        ascii_uppercase,
        digits,
        ascii_lowercase
    ]
    passphrase = ""
    good_passphrase_found = False
    while not good_passphrase_found:
        passphrase = "".join([choice(choice(choices)) for x in range(pass_len)])
        if match(r"^(([a-z0-9])\2?(?!\2))+$", passphrase, MULTILINE):
            good_passphrase_found = True
            if userid and userid in passphrase:
                 good_passphrase_found = False
    return passphrase

class FilterModule(object):
    """ Jinja2 filters for generating passwords """

    def filters(self):
        filters = {
            "generate_password": generate_password,
            "generate_passphrase": generate_passphrase,
        }
        return filters
