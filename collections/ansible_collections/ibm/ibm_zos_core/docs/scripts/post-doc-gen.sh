#!/bin/sh

################################################################################
# © Copyright IBM Corporation 2020
################################################################################

################################################################################
# Script and actions called after generating doc, helpful in cases where
# customization is needed but does not apply to all collections
################################################################################
SCRIPT_PATH=`dirname "$0"`
DOC_DIR=`( cd "$SCRIPT_PATH" && pwd )| sed 's|\(.*\)/.*|\1|'`
# `pwd| sed 's|\(.*\)/.*|\1|'`

################################################################################
# Delete the playbook-single.rst
################################################################################

if [[ -f $DOC_DIR/source/playbooks-single.rst  ]]; then
    #echo "Deleting file [$DOC_DIR/source/playbooks-single.rst]";
    rm -rf $DOC_DIR/source/playbooks-single.rst
fi

################################################################################
# Delete the requirements-single.rst
################################################################################

if [[ -f $DOC_DIR/source/requirements-single.rst  ]]; then
    #echo "Deleting file [$DOC_DIR/source/requirements-single.rst]";
    rm -rf $DOC_DIR/source/requirements-single.rst
fi

################################################################################
# Replace the temporary index
################################################################################

if [[ -f $DOC_DIR/source/index.org  ]]; then
    #echo "Replace [$DOC_DIR/source/index.rst] with [$DOC_DIR/source/index.org]"
    mv $DOC_DIR/source/index.org $DOC_DIR/source/index.rst
fi
