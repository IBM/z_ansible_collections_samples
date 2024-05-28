# -*- coding: utf-8 -*-
################################################################################
# © Copyright IBM Corporation 2023
################################################################################
import os

print("This script lists the files contained in the current working directory")

cwd = os.getcwd()
current_files = os.listdir(cwd)

print("Files in {0}:\n{1}".format(cwd, current_files))
