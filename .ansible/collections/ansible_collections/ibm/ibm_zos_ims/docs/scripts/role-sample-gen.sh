################################################################################
# © Copyright IBM Corporation 2020
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
################################################################################

################################################################################
# Script will set up a sample Ansible role, add the necessary required
# metadata so that doc can be generated, supply a document on how to write
# metadata, run the sample and generate role documentation if you have the
# documentation requirements:
# pip show ansible-doc-extractor
# pip show ansible
# pip show sphinx-rtd-theme
# pip show sphinx-versions
# pip show Sphinx
#
# Use `git status` to see which files have been created and which ones you
# should keep.
################################################################################

################################################################################
# Script Vars
################################################################################

_SCRIPTS="/scripts"
_DOC_SCRIPTS="/docs/scripts"
_ROLE_NAME="role_sample"



# SCRIPT_PATH /Users/xxx/git/github/ibm_zos_core/docs/scripts
SCRIPT_PATH="$( cd "$( dirname "$0" )" >/dev/null 2>&1 && pwd )"

# PROJECT_ROOT_PATH /Users/xxx/git/github/ibm_zos_core
PROJECT_ROOT_PATH="${SCRIPT_PATH/${_DOC_SCRIPTS}/}"

# ROLE_SAMPLE_FILES_PATH /Users/ddimatos/git/github/ibm_zos_core/docs/files/role_sample
ROLE_SAMPLE_FILES_PATH="${SCRIPT_PATH/${_SCRIPTS}/}"/files/role_sample

# ROLE_ROOT /Users/xxx/git/github/ibm_zos_core/roles
ROLE_ROOT="${PROJECT_ROOT_PATH}"/roles

# ROLE_PATH /Users/xxx/git/github/ibm_zos_core/roles/role_sample
ROLE_PATH=${ROLE_ROOT}/${_ROLE_NAME}

# PROJECT_DOC_SOURCE_PATH /Users/xxx/git/github/ibm_zos_core/docs/source
PROJECT_DOC_SOURCE_PATH="${PROJECT_ROOT_PATH}"/docs/source

echo SCRIPT_PATH $SCRIPT_PATH
echo PROJECT_ROOT_PATH $PROJECT_ROOT_PATH
echo ROLE_SAMPLE_FILES_PATH $ROLE_SAMPLE_FILES_PATH
echo ROLE_ROOT $ROLE_ROOT
echo ROLE_PATH $ROLE_PATH

# Does ROLE_ROOT_PATH (/Users/xxx/git/github/ibm_zos_core/roles) exist mkidr -p $ROLE_ROOT_PATH
if [ ! -d "$ROLE_ROOT" ]
then
    echo "Created directory $ROLE_ROOT"
    mkdir -p $ROLE_ROOT
fi

if [ ! -d "$ROLE_PATH" ]
then
    # cd to ROLE_ROOT and create a role project with ansible-galaxy init $_ROLE_NAME
    echo "Creating $_ROLE_NAME in $ROLE_ROOT"
    cd $ROLE_ROOT
    ansible-galaxy init $_ROLE_NAME
    mkdir -p $ROLE_PATH/docs

    # Copy role artifacts to generated role sample
    echo "Populating $_ROLE_NAME with sample artifacts"

    # cp ROLE_SAMPLE_FILES_PATH/defaults-main.yml to ROLE_PATH/defaults/main.yml
    echo "$ROLE_SAMPLE_FILES_PATH/defaults-main.yml to $ROLE_PATH/defaults/main.yml"
    cp $ROLE_SAMPLE_FILES_PATH/defaults-main.yml $ROLE_PATH/defaults/main.yml

    # cp ROLE_SAMPLE_FILES_PATH/meta-main.yml to ROLE_PATH/meta/main.yml
    echo "Copy $ROLE_SAMPLE_FILES_PATH/meta-main.yml to $ROLE_PATH/meta/main.yml"
    cp $ROLE_SAMPLE_FILES_PATH/meta-main.yml $ROLE_PATH/meta/main.yml

    # cp ROLE_SAMPLE_FILES_PATH/tasks-main.yml to ROLE_PATH/tasks/main.yml
    echo "Copy $ROLE_SAMPLE_FILES_PATH/tasks-main.yml to $ROLE_PATH/tasks/main.yml"
    cp $ROLE_SAMPLE_FILES_PATH/tasks-main.yml $ROLE_PATH/tasks/main.yml

    # cp ROLE_SAMPLE_FILES_PATH/vars-main.yml to ROLE_PATH/vars/main.yml
    echo "Copy $ROLE_SAMPLE_FILES_PATH/vars-main.yml to $ROLE_PATH/vars/main.yml"
    cp $ROLE_SAMPLE_FILES_PATH/vars-main.yml $ROLE_PATH/vars/main.yml

    # cp ROLE_SAMPLE_FILES_PATH/docs-doc_role_sample to ROLE_PATH/docs
    echo "Copy $ROLE_SAMPLE_FILES_PATH/docs_doc_role_sample to $ROLE_PATH/docs"
    cp $ROLE_SAMPLE_FILES_PATH/doc_role_sample $ROLE_PATH/docs

    # cp ROLE_SAMPLE_FILES_PATH/$_ROLE_NAME.yml to ROLE_PATH
    echo "Copy $ROLE_SAMPLE_FILES_PATH/$_ROLE_NAME.yml to $ROLE_PATH"
    cp $ROLE_SAMPLE_FILES_PATH/$_ROLE_NAME.yml $ROLE_PATH

    # cp ROLE_SAMPLE_FILES_PATH/role-doc-readme.txt to ROLE_PATH
    echo "Copy $ROLE_SAMPLE_FILES_PATH/role-doc-readme.txt to $ROLE_PATH"
    cp $ROLE_SAMPLE_FILES_PATH/role-doc-readme.txt $ROLE_PATH
else
    echo "$_ROLE_NAME already exists in $ROLE_ROOT, no changes made."
fi

# Does $ROLE_SAMPLE_FILES_PATH/roles.rst exist in PROJECT_DOC_SOURCE_PATH
if [ ! -e "$PROJECT_DOC_SOURCE_PATH"/roles.rst ]
then
    echo "Copy $ROLE_SAMPLE_FILES_PATH/roles.rst to $PROJECT_DOC_SOURCE_PATH"
    cp $ROLE_SAMPLE_FILES_PATH/roles.rst $PROJECT_DOC_SOURCE_PATH
fi

if [ -e $ROLE_PATH/$_ROLE_NAME.yml ]
then
    echo "Execute playbook for sample role."
    ansible-playbook $ROLE_PATH/$_ROLE_NAME.yml
fi

packages=("ansible-doc-extractor" "ansible" "sphinx-rtd-theme" "sphinx-versions" "Sphinx")

printf 'Checking for required %s package.\n' "${packages[@]}"

for pkg in ${packages[@]}; do
    PIP_PKG=`pip show $pkg|grep Version:`
	if [ "$PIP_PKG" == "" ]; then
		echo "$pkg is not installed, will not be able to generate documentation";
        echo "Try 'pip install $pkg' and rerun the script";
        exit;
	fi
done

if [ -e $PROJECT_DOC_SOURCE_PATH/index.rst ]
then
    # Check that $PROJECT_DOC_SOURCE_PATH/index.rst contains roles in the toctree
    echo "Found $PROJECT_DOC_SOURCE_PATH/index.rst, checking if it has a role entry.";

    ROLES_TOC=`cat $PROJECT_DOC_SOURCE_PATH/index.rst |grep -w "roles"`
    # echo ROLES_TOC $ROLES_TOC

    if [ "$ROLES_TOC" == "" ]; then
		echo "Was unable to find roles defined in a toctree in $PROJECT_DOC_SOURCE_PATH/index.rst";
        echo "Edit $PROJECT_DOC_SOURCE_PATH/index.rst and add roles to a toctree for example:";
        echo ".. toctree::";
        echo "   :maxdepth: 1";
        echo "   :caption: Ansible Content";
        echo "";
        echo "   plugins";
        echo "   modules";
        echo "   filters";
        echo "   roles";
        echo "When roles has been added to $PROJECT_DOC_SOURCE_PATH/index.rst press [Enter]";
        read -p "Press [Enter] key to continue...";
	fi
else
    echo "Was unable to find $PROJECT_DOC_SOURCE_PATH/index.rst, unable to continue to documentation generation.";
    echo "Please create the $PROJECT_DOC_SOURCE_PATH/index.rst and run commands:";
    echo "cd ${PROJECT_ROOT_PATH}/docs/";
    echo "make clean;make role-doc;make html;make view-html;";
fi

echo "Generate documentation for sample "
cd ${PROJECT_ROOT_PATH}/docs/
make clean;make role-doc;make html;make view-html;

