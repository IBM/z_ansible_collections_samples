# Copyright 2017-2020 IBM Corp. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

import os
import sys
import re
import subprocess
import git  # GitPython package


def get_docs_tags(min_version):
    """
    Get the list of Git tags that should be included in the documentation.

    The algorithm uses those tags that match the format M.N.U, and that are
    equal to or higher than the specified minimum version.

    As a result, all update version of a particular minor version are built,
    e.g. 0.9.0, 0.9.1, and 0.9.2. A prior approach built only the latest
    update version of a particular minor version, i.e. only 0.9.2 in this
    example, effectively removing the docs for 0.9.1 when 0.9.2 was released.
    While that approach is more economic in terms of number of versions,
    it makes it hard for users to reference the documentation, since a
    version they have referenced today may no longer exist tomorrow, just
    because a fix was released.

    Parameters:

      min_version (string): Minimum version to use.

    Returns:

      tuple of strings: List of Git tags to use.
    """
    min_version_tuple = tuple(map(int, min_version.split('.')))
    repo_dir = os.path.join(os.path.dirname(__file__), '..', '..')
    try:
        repo = git.Repo(repo_dir)
    except git.InvalidGitRepositoryError:
        # sphinx-versioning runs the conf.py file once on the checked out
        # repo to determine the scv_* parameters to use, and then sphinx-build
        # runs it again when building each tag/branch. The first run succeeds
        # because that is done on the checked out repo. The subsequent runs
        # raise this exception because sphinx-versioning invoked sphinx-build
        # on a copy of the repo that does not include the .git subtree.
        # However, since the sphinx-build runs do not look at the scv_*
        # parameters, we can ignore the exception and return anything, e.g.
        # an empty tuple.
        return tuple()

    tag_names = []
    for tag in repo.tags:
        m = re.match(r'^(\d+)\.(\d+)\.(\d+)$', tag.name)
        if m:
            tag_version_tuple = tuple(map(int, tag.name.split('.')))
            if tag_version_tuple >= min_version_tuple:
                tag_names.append(tag.name)
    return tuple(tag_names)


def get_docs_branches():
    """
    Get the list of Git branches that should be included in the documentation.

    The algorithm uses only 'master' (the development branch).

    A prior approach uses also the latest fix branch 'stable_M.N', but that
    is too confusing for only the small gain that upcoming fixes are already
    documented before being released (which should not take long, anyway),
    plus it has the same problem as described in get_docs_tags() which is
    that the release of a new major or minor version increases the latest
    fix branch, causing the docs for the previous fix branch to go away.

    Returns:

      tuple of strings: List of Git branches to use.
    """
    return ('master',)


# -- Project information -----------------------------------------------------

# The full version, including alpha/beta/rc tags
_version_file = '../../tools/version.py'  # relative to the dir of this file
_version_file = os.path.relpath(os.path.join(
    os.path.dirname(__file__), _version_file))
version = subprocess.check_output(
    '{0} {1}'.format(sys.executable, _version_file), shell=True).decode("utf-8")

project = 'IBM Z HMC collection'
copyright = '2016-2020, IBM'
author = 'IBM'

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "sphinx_rtd_theme",
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

html_theme_options = {'display_version': True}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']

# do not copy the source RST files into the generated content
html_copy_source = False

# and do not show the links to the originals
html_show_sourcelink = False

##############################################################################
#                          sphinx-versioning                                 #
##############################################################################
# Options for sphinx-versioning 1.0.0
# https://pypi.org/project/sphinx-versions/1.0.0/
#
# Documentation for v1.0.0
# https://github.com/Smile-SA/sphinx-versions/blob/v1.0.0/docs/settings.rst
#
# For more information on sphinx-versioning options follow this link:
# https://sphinx-versions.readthedocs.io/en/latest/settings.html#cmdoption-arg-destination
#
# Commands:
# sphinx-versioning -l docs/source/conf.py build docs/source/ docs/build/html
# open docs/build/html/v1.1.0-beta1/index.html
##############################################################################

# Give the underlying ``sphinx-build`` program command line options.
# ``sphinx-versions`` passes everything after ``--`` to it ``sphinx-build`` and
# in this case we are wanting to disable the sphinx footer.
# NOTE:  Appending "-- -D html_show_sphinx=False" to the Makefile
# ``sphinx-versioning`` command nor the ``scv_overflow`` are working.
scv_overflow = ("-D", "html_show_sphinx=False")

# List of Github branches that are included as versions in the documentation.
# This is in addition to the 'scv_whitelist_tags' option.
# The minimum version must be the first version that was released to Ansible Galaxy.
scv_whitelist_branches = get_docs_branches()

# The Github branch or tag that will be used as the version that is shown for
# the root URI of the documentation site.
# scv_root_ref = 'master'

# Override the 'scv_root_ref' option to use the tag with the highest version
# number. If no tags have docs then this option is ignored and 'scv_root_ref'
# is used. Default is False.
scv_greatest_tag = True

# Override the 'scv_root_ref' option to use the most recently committed tag.
# If no tags have docs then this option is ignored and 'scv_root_ref' is used.
# Default is False.
# scv_recent_tag = True

# List of Github tags that are included as versions in the documentation.
# This is in addition to the 'scv_whitelist_branches' option.
# The minimum version must be the first version that was released to Ansible Galaxy.
scv_whitelist_tags = get_docs_tags(min_version='0.9.0')

# Sort versions by one or more values. Valid values are semver, alpha, and time.
# Semantic is referred to as 'semver', this would ensure our latest VRM is
# the first in the list of documentation links.
scv_sort = ('semver',)

# Show a warning banner about not using the latest version. Default is False.
# Further info:
# https://sphinx-versions.readthedocs.io/en/latest/banner.html#banner
# This documentation sets this to False because only ony version can have no
# banner, so it will be either on the latest released tagged version or on the
# master branch, and having the banner appear on either of them is confusing in
# a way.
# scv_show_banner = False

# The Github branch or tag considered to be the latest version for purposes
# of the warning banner. The banner will not be displayed in this ref, only in
# all others. Default is master.
# This can be overridden with the 'scv_banner_greatest_tag' option.
# scv_banner_main_ref = 'master'

# Override the 'scv_banner_main_ref' option to automatically use the tag with
# the highest version number. If no tags have docs then this option is ignored
# and 'scv_banner_main_ref' is used. Default is False.
# scv_banner_greatest_tag = True

# Override the 'scv_banner_main_ref' option to use the most recently committed
# tag. If no tags have docs then this option is ignored and
# 'scv_banner_main_ref' is used. Default is False.
# scv_banner_recent_tag = True

# Invert the order of branches/tags displayed in the sidebars in generated HTML
# documents. The default order is whatever git prints when
# running "git ls-remote --tags ./."
scv_invert = True
