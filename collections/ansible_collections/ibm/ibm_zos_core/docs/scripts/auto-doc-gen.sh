#!/bin/sh

################################################################################
# © Copyright IBM Corporation 2020
################################################################################

################################################################################
# Script and actions called before generating doc, helpful in cases where
# customization is needed but does not apply to all collections
################################################################################
SCRIPT_PATH=`dirname "$0"`
DOC_DIR=`( cd "$SCRIPT_PATH" && pwd )| sed 's|\(.*\)/.*|\1|'`
# `pwd| sed 's|\(.*\)/.*|\1|'`

################################################################################
# Source the playbook*.rst into a single RST so that our topic is a single html
################################################################################

################################################################################
# Deprecated playbooks-single.rst
# # If playbooks-single.rst exists, zero it out so we generate the lastest based
# # on the payboook-*.rst files
# if [[ -f $DOC_DIR/source/playbooks-single.rst  ]]; then
#     :> $DOC_DIR/source/playbooks-single.rst
# fi

# # Remove the first toctree entry and onwards from playbooks.rst to create a new
# # RST playbooks-single.rst. Note, you should ensure toctree entries are at the
# # end of the conent to not remove unwanted content.
# if [[ -f $DOC_DIR/source/playbooks.rst ]]; then

#     # Remove the toctree entries
#     awk '/toctree/ {exit} {print}' $DOC_DIR/source/playbooks.rst >> $DOC_DIR/source/playbooks-single.rst

#     # Concat the interested RSTs into playbooks-single.rst to create a single RST
#     if [[ -f $DOC_DIR/source/playbook_config_setup.rst && -f $DOC_DIR/source/playbook_inventory.rst && -f $DOC_DIR/source/playbook_group_vars.rst && -f $DOC_DIR/source/playbook_run.rst ]]; then

#         # Inform readers this is auto generated thus avoding the need to maintain
#         echo ".. ...........................................................................\n$(<$DOC_DIR/source/playbooks-single.rst)" > $DOC_DIR/source/playbooks-single.rst
#         echo ".. Auto generated restructured text                                          .\n$(<$DOC_DIR/source/playbooks-single.rst)" > $DOC_DIR/source/playbooks-single.rst
#         echo ".. ...........................................................................\n$(<$DOC_DIR/source/playbooks-single.rst)" > $DOC_DIR/source/playbooks-single.rst

#         # For each identified file we want to merge into playbooks-single.rst cat them and merge
#         for file in $DOC_DIR/source/playbook_config_setup.rst $DOC_DIR/source/playbook_inventory.rst $DOC_DIR/source/playbook_group_vars.rst $DOC_DIR/source/playbook_run.rst; do
#             echo "" >> $DOC_DIR/source/playbooks-single.rst;
#             cat "$file" >> $DOC_DIR/source/playbooks-single.rst;
#         done
#     else
#         # When unable to merge remove the auto generated RST so that is apparent
#         # it is not generated and will diff in a 'git status'

#         rm -rf $DOC_DIR/source/playbooks-single.rst
#     fi
# fi
################################################################################

# If requirements-single.rst exists, zero it out so we generate the lastest based
# on the requirements-*.rst files
if [[ -f $DOC_DIR/source/requirements-single.rst  ]]; then
    :> $DOC_DIR/source/requirements-single.rst
fi

# Remove the first toctree entry and onwards from playbooks.rst to create a new
# RST requirements-single.rst. Note, you should ensure toctree entries are at
# the end of the conent to not remove unwanted content.
if [[ -f $DOC_DIR/source/requirements.rst ]]; then

    # Remove the toctree entries
    awk '/toctree/ {exit} {print}' $DOC_DIR/source/requirements.rst >> $DOC_DIR/source/requirements-single.rst

    # Concat the interested RSTs into requirements-single.rst to create a single RST
    if [[ -f $DOC_DIR/source/requirements_managed.rst ]]; then

        # Inform readers this is auto generated thus avoding the need to maintain
        echo ".. ...........................................................................\n$(<$DOC_DIR/source/requirements-single.rst)" > $DOC_DIR/source/requirements-single.rst
        echo ".. Auto generated restructured text                                          .\n$(<$DOC_DIR/source/requirements-single.rst)" > $DOC_DIR/source/requirements-single.rst
        echo ".. ...........................................................................\n$(<$DOC_DIR/source/requirements-single.rst)" > $DOC_DIR/source/requirements-single.rst

        # For each identified file we want to merge into requirements-single.rst cat them and merge
        for file in $DOC_DIR/source/requirements_managed.rst; do
            cat "$file" >> $DOC_DIR/source/requirements-single.rst;
        done
    else
        # When unable to merge remove the auto generated RST so that is apparent
        # it is not generated and will diff in a 'git status'
        rm -rf $DOC_DIR/source/requirements-single.rst
    fi
fi
