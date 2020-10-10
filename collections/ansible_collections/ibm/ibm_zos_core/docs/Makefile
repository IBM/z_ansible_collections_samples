##############################################################################
# © Copyright IBM Corporation 2020                                           #
##############################################################################

# You can set these variables from the command line, and also
# from the environment for the first two.
SPHINXOPTS    ?=
SPHINXBUILD   ?= sphinx-build
SOURCEDIR     = source
BUILDDIR      = build
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))


view-html:
	@echo "Display generated HTML in default browser."
	@open build/html/index.html

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

clean:
	@rm -rf build
	@echo "Deleted directory 'build/'."

	@rm -rf source/modules
	@echo "Deleted directory source/modules."

	@if test -e ../plugins/modules/__init__.py.skip; \
	then mv -f ../plugins/modules/__init__.py.skip ../plugins/modules/__init__.py; \
	fi
	@echo "Reset '../plugins/modules/__init__.py'"

	@echo "Deleted directory '../plugins/modules/rexx_module_doc/'."
	@rm -rf ../plugins/modules/rexx_module_doc

	@echo "Completed cleanup, run 'make module_doc'."

module-doc:
	@mkdir build
	@echo "Make build directory for Sphinx generated HTML."

	@mkdir -p source/modules
	@echo "Make source/modules for module RST generated doc."

	@mv ../plugins/modules/__init__.py ../plugins/modules/__init__.py.skip
	@echo "Move file '../plugins/modules/__init__.py' to a tmp location to avoid failure'"

	@echo "Extracting REXX module ansible-doc and generating RST"
	@mkdir -p ../plugins/modules/rexx_module_doc

	@for rexx_module in `ls ../plugins/modules/*rexx`; do\
	   REXX_FILE=`basename $$rexx_module .rexx`; \
	   echo Extracting doc for module $$REXX_FILE; \
	   touch ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   sed -n "/DOCUMENTATION = '''/,/'''/p" ../plugins/modules/$$REXX_FILE.rexx >> ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   sed -n "/EXAMPLES = '''/,/'''/p" ../plugins/modules/$$REXX_FILE.rexx >> ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   sed -n "/RETURN = '''/,/'''/p" ../plugins/modules/$$REXX_FILE.rexx >> ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   echo Generating RST doc for module $$REXX_FILE; \
	   ansible-doc-extractor --template templates/module.rst.j2 source/modules ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	done

	@echo "Deleted directory '../plugins/modules/rexx_module_doc/'."
	@rm -rf ../plugins/modules/rexx_module_doc

	@ansible-doc-extractor --template templates/module.rst.j2 source/modules ../plugins/modules/*.py
	@echo "Extract module ansible doc from '../plugins/modules/' into 'source/modules'"

	@if test -e ../plugins/modules/__init__.py.skip; \
	then mv -f ../plugins/modules/__init__.py.skip ../plugins/modules/__init__.py; \
	fi
	@echo "Reset '../plugins/modules/__init__.py'"

	@echo "Completed restructured text generation, run 'make html'"

pre:
	@$(shell scripts/auto-doc-gen.sh)
	@echo "Completed auto-doc-generation see script auto-doc-gen.sh for details"

html:

	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	@echo "Completed HTML generation"

html-all:

	# @sphinx-versioning -l "$(ROOT_DIR)"/source/conf.py build  "$(ROOT_DIR)"/source/ "$(ROOT_DIR)"/build/html -- -D html_show_sphinx=False
	@sphinx-versioning -l "$(ROOT_DIR)"/source/conf.py build  "$(ROOT_DIR)"/source/ "$(ROOT_DIR)"/build/html
	@echo "Completed HTML generation for git repository branches and/or tags, run 'make view-html'"

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	@echo "Completed HTML text generation, run 'make view-html'"
