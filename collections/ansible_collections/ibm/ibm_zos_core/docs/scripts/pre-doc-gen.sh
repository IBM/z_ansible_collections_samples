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

# Delete any temporary index RST
if [[ -f $DOC_DIR/source/index-temp.rst  ]]; then
    #echo "Deleting file [$DOC_DIR/source/index-temp.rst]"
    rm -rf $DOC_DIR/source/index-temp.rst

    if [[ -f $DOC_DIR/source/index.org ]]; then
        #echo "Moving file [$DOC_DIR/source/index.org] to [$DOC_DIR/source/index.rst]"
        mv $DOC_DIR/source/index.org $DOC_DIR/source/index.rst
    fi
fi

# Remove all the toctree entries from playbooks.rst as a new RST playbooks-single.rst
if [[ -f $DOC_DIR/source/playbooks.rst ]]; then
    #echo "Creating [$DOC_DIR/source/playbooks-single.rst] with no toctree entries."

    # Remove the toctree entries
    awk '/toctree/ {exit} {print}' $DOC_DIR/source/playbooks.rst >> $DOC_DIR/source/playbooks-single.rst

    # Concat the interested RSTs into playbooks-single.rst to create a single RST
    if [[ -f $DOC_DIR/source/playbook_config_setup.rst && -f $DOC_DIR/source/playbook_inventory.rst && -f $DOC_DIR/source/playbook_group_vars.rst && -f $DOC_DIR/source/playbook_run.rst ]]; then

        for file in $DOC_DIR/source/playbook_config_setup.rst $DOC_DIR/source/playbook_inventory.rst $DOC_DIR/source/playbook_group_vars.rst $DOC_DIR/source/playbook_run.rst; do
            #echo "Merging $file into [$DOC_DIR/source/playbooks-single.rst]."
            cat "$file" >> $DOC_DIR/source/playbooks-single.rst;
        done

        # Update the temporary index-temp.rst toctree with a new RST playbooks-single entry
        if [[ -f $DOC_DIR/source/playbooks-single.rst ]]; then
            #echo "Updating [index-temp.rst] with toctree entry [$DOC_DIR/source/playbooks-single.rst]."
            awk '{ sub(/^   playbooks$/,"   playbooks-single"); print }' $DOC_DIR/source/index.rst > $DOC_DIR/source/index-temp.rst
        fi
    else
        #echo "Unable to merge playbook*.rst files a single [playbooks-single.rst]."

        #echo "Removing [$DOC_DIR/source/playbooks-single.rst]."
        rm -rf $DOC_DIR/source/playbooks-single.rst

        #echo "Removing [$DOC_DIR/source/index-temp.rst]."
        rm -rf $DOC_DIR/source/index-temp.rst
    fi
#else
#    #echo "Unable to create single source RST for [$DOC_DIR/source/playbooks.rst]."
fi

# Remove all the toctree entries from requirements.rst as a new RST requirements-single.rst
if [[ -f $DOC_DIR/source/requirements.rst ]]; then
    #echo "Creating [$DOC_DIR/source/requirements-single.rst] with no toctree entries."

    # Remove the toctree entries
    awk '/toctree/ {exit} {print}' $DOC_DIR/source/requirements.rst >> $DOC_DIR/source/requirements-single.rst

    # Concat the interested RSTs into requirements-single.rst to create a single RST
    if [[ -f $DOC_DIR/source/requirements_managed.rst ]]; then

        for file in $DOC_DIR/source/requirements_managed.rst; do
            #echo "Merging $file into [$DOC_DIR/source/requirements-single.rst]."
            cat "$file" >> $DOC_DIR/source/requirements-single.rst;
        done

### we need a check if index-temp here else do ...
        # Update the temporary index-temp.rst toctree with a new RST playbooks-single entry
        if [[ -f $DOC_DIR/source/requirements-single.rst ]]; then
            # echo "Updating [index-temp.rst] with toctree entry [$DOC_DIR/source/requirements-single.rst]."

            if [[ -f $DOC_DIR/source/index-temp.rst ]]; then
                awk '{ sub(/^   requirements$/,"   requirements-single"); print }' $DOC_DIR/source/index-temp.rst > $DOC_DIR/source/index-temp-temp.rst
                mv $DOC_DIR/source/index-temp-temp.rst $DOC_DIR/source/index-temp.rst
            else
                awk '{ sub(/^   requirements$/,"   requirements-single"); print }' $DOC_DIR/source/index.rst > $DOC_DIR/source/index-temp.rst
            fi
        fi
    else
        #echo "Unable to merge requirements*.rst files a single [requirements-single.rst]."

        #echo "Removing [$DOC_DIR/source/requirements-single.rst]."
        rm -rf $DOC_DIR/source/requirements-single.rst

        #echo "Removing [$DOC_DIR/source/index-temp.rst]."
        rm -rf $DOC_DIR/source/index-temp.rst
    fi
#else
#    #echo "Unable to create single source RST for [$DOC_DIR/source/playbooks.rst]."
fi

# Backup the original index.rst (without rst extenstion) and move the temporary one
if [[ -f $DOC_DIR/source/index-temp.rst  ]]; then
    #echo "Backing up [$DOC_DIR/source/index.rst] as [$DOC_DIR/source/index.org]"
    mv $DOC_DIR/source/index.rst $DOC_DIR/source/index.org

    #echo "Replacing [$DOC_DIR/source/index-temp.rst] as [$DOC_DIR/source/index.rst]"
    mv $DOC_DIR/source/index-temp.rst $DOC_DIR/source/index.rst
fi