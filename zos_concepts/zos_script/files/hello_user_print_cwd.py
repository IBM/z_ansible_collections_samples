# -*- coding: utf-8 -*-
###############################################################################
# © Copyright IBM Corporation 2024
###############################################################################

import argparse, os

# parse 'name' variable as a command line arg.
parser = argparse.ArgumentParser()
parser.add_argument("--name", help="User's Name.")
args = parser.parse_args()

# print the value of the parsed arg 'name'.
print("Hello {0}.".format(args.name))

# print the current working directory.
print("This python script was run inside directory:", os.getcwd())