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
line_header="===================================================================="


view-html:
	@echo "Display generated HTML in default browser."
	@open build/html/index.html

# Put it first so that "make" without argument is like "make help".
help:
	@echo $(line_header)
	@echo "Examples:"
	@echo "$(line_header)"
	@echo "  make clean;make role-doc;make html;make view-html;"
	@echo "  make clean;make module-doc;make html;make view-html;"
	@echo "  make clean;make module-doc;make role-doc;make html;make view-html;"
	@echo "  make clean;"

	@echo $(line_header)
	@echo "Make file targets"
	@echo "$(line_header)"
	@echo "clean - will remove unnecessary documentation and generated documentation artifacts:"
	@echo "      - deletes $(ROOT_DIR)/build"
	@echo "      - deletes $(ROOT_DIR)/source/modules"
	@echo "      - deletes $(ROOT_DIR)/source/roles"
	@echo "      - deletes ../plugins/modules/rexx_module_doc"
	@echo "      - resets __init__.py.skip"
	@echo "      - deletes $(ROOT_DIR)/build"
	@echo "role-doc - scans collection for roles; generate RST from role metadata."
	@echo "module-doc - scans collection for modules; generate RST for REXX and python modules."
	@echo "pre - <deprecated> Combines multiple RST int one, invokes external script."
	@echo "html - generates HTML to $(ROOT_DIR)/build/html."
	@echo "version-html - generates HTML with versions, requires necessary dependencies."
	@echo "view-html - displays generated HTML in your default browser."

	@echo $(line_header)
	@echo "Sphinx target type options"
	@echo $(line_header)
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile

clean:
	@echo $(line_header)
	@echo "Running Target clean"
	@echo $(line_header)

	@if test -d build; then \
	echo "Deleting directory '$(ROOT_DIR)/build'."; \
	rm -rf build; \
	fi

	@if test -d source/modules; then \
	echo "Deleting directory '$(ROOT_DIR)/source/modules'."; \
	rm -rf source/modules; \
	fi

	@if test -d source/roles; then \
	echo "Deleting directory '$(ROOT_DIR)/source/roles'."; \
	rm -rf source/roles; \
	fi

	@if test -d ../plugins/modules/rexx_module_doc; then \
	echo "Deleting directory '../plugins/modules/rexx_module_doc'."; \
	rm -rf ../plugins/modules/rexx_module_doc; \
	fi

	@if test -e ../plugins/modules/__init__.py.skip; then \
	echo "Reset __init__.py.skip for '../plugins/modules/__init__.py'"; \
	mv -f ../plugins/modules/__init__.py.skip ../plugins/modules/__init__.py; \
	fi

	@echo "Completed cleanup, run 'make module-doc' or 'make role-doc'."

role-doc:
	@echo $(line_header)
	@echo "Running Target role-doc"
	@echo $(line_header)

	@if ! test -d build; then \
	mkdir build; \
    echo "Make $(ROOT_DIR)/build directory for Sphinx generated HTML."; \
	fi

	@if ! test -d source/roles; then \
	mkdir -p source/roles; \
    echo "Make $(ROOT_DIR)/source/roles directory for Sphinx generated HTML."; \
	fi

	@for role_name in `ls ../roles/`; do \
	echo "Detected role name $$role_name"; \
	if test -e ../roles/$$role_name/docs/doc_$$role_name; then \
	echo "Copying metadata ../roles/$$role_name/docs/doc_$$role_name to ../roles/$$role_name/docs/doc_$$role_name.py"; \
	cp ../roles/$$role_name/docs/doc_$$role_name ../roles/$$role_name/docs/doc_$$role_name.py; \
	echo "Sanitizing metadata ../roles/$$role_name/docs/doc_$$role_name.py"; \
	sed -i "" "s/role:/module:/g" ../roles/$$role_name/docs/doc_$$role_name.py; \
	echo "Generating RST doc for role ../roles/$$role_name"; \
	ansible-doc-extractor --template templates/role.rst.j2 source/roles ../roles/$$role_name/docs/doc_$$role_name.py; \
	echo "Deleting generated metadata ../roles/$$role_name/docs/doc_$$role_name.py"; \
	rm -rf ../roles/$$role_name/docs/doc_$$role_name.py; \
	fi; \
	done

	@echo $(line_header)
	@echo "Completed ReStructuredText generation for roles; next run 'make html'"
	@echo $(line_header)

module-doc:
	@echo $(line_header)
	@echo "Running Target module-doc"
	@echo $(line_header)

	@if ! test -d build; then \
	echo "Make $(ROOT_DIR)/build directory for Sphinx generated HTML."; \
	mkdir build; \
	fi

	@if ! test -d source/modules; then \
	echo "Make $(ROOT_DIR)/source/modules directory for Sphinx generated HTML."; \
	mkdir -p source/modules; \
	fi

	@if ! test -d ../plugins/modules/rexx_module_doc; then \
	echo "Make ../plugins/modules/rexx_module_doc directory to extract REXX doc into temporary file."; \
	mkdir -p ../plugins/modules/rexx_module_doc; \
	else \
	echo "Delete ../plugins/modules/rexx_module_doc directory used to extract REXX doc into temporary file."; \
	rm -rf ../plugins/modules/rexx_module_doc; \
	echo "Make ../plugins/modules/rexx_module_doc directory to extract REXX doc into temporary file."; \
	mkdir -p ../plugins/modules/rexx_module_doc; \
	fi

	@for rexx_module in `ls ../plugins/modules/*rexx`; do\
	   REXX_FILE=`basename $$rexx_module .rexx`; \
	   echo "Extracting documentation for module $$REXX_FILE into ../plugins/modules/rexx_module_doc/$$REXX_FILE.py"; \
	   touch ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   sed -n "/DOCUMENTATION = '''/,/'''/p" ../plugins/modules/$$REXX_FILE.rexx >> ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   sed -n "/EXAMPLES = '''/,/'''/p" ../plugins/modules/$$REXX_FILE.rexx >> ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   sed -n "/RETURN = '''/,/'''/p" ../plugins/modules/$$REXX_FILE.rexx >> ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	   echo "Generating ReStructuredText for module $$REXX_FILE inot source/modules/$$REXX_FILE.rst"; \
	   ansible-doc-extractor --template templates/module.rst.j2 source/modules ../plugins/modules/rexx_module_doc/$$REXX_FILE.py; \
	done

	@if test -d ../plugins/modules/rexx_module_doc; then \
	echo "Delete ../plugins/modules/rexx_module_doc directory used to extract REXX doc into temporary file."; \
	rm -rf ../plugins/modules/rexx_module_doc; \
	fi

	@if test -e ../plugins/modules/__init__.py; then \
	echo "Rename file '../plugins/modules/__init__.py' to ../plugins/modules/__init__.py.skip to avoid reading empty python file.'"; \
	mv ../plugins/modules/__init__.py ../plugins/modules/__init__.py.skip; \
	fi

	@echo "Generating ReStructuredText for all ansible modules found at '../plugins/modules/' to 'source/modules'."
	@ansible-doc-extractor --template templates/module.rst.j2 source/modules ../plugins/modules/*.py

	@if test -e ../plugins/modules/__init__.py.skip; \
    echo "Rename file '../plugins/modules/__init__.py.skip' back to ../plugins/modules/__init__.py.'"; \
	then mv -f ../plugins/modules/__init__.py.skip ../plugins/modules/__init__.py; \
	fi

	@echo $(line_header)
	@echo "Completed ReStructuredText generation for modules; next run 'make html'"
	@echo $(line_header)

pre:
	@echo $(line_header)
	@echo "Running Target pre"
	@echo $(line_header)

	@$(shell scripts/auto-doc-gen.sh)

	@echo $(line_header)
	@echo "Completed auto-doc-generation see script auto-doc-gen.sh for details"
	@echo $(line_header)

html:
	@echo $(line_header)
	@echo "Running Target html"
	@echo $(line_header)

	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

	@echo $(line_header)
	@echo "Completed HTML generation, see $(ROOT_DIR)/build/html; next run 'make view-html' "
	@echo $(line_header)

version-html:
	@echo $(line_header)
	@echo "Running Target version-html"
	@echo $(line_header)

	# @sphinx-versioning -l "$(ROOT_DIR)"/source/conf.py build  "$(ROOT_DIR)"/source/ "$(ROOT_DIR)"/build/html -- -D html_show_sphinx=False
	@sphinx-versioning -l "$(ROOT_DIR)"/source/conf.py build  "$(ROOT_DIR)"/source/ "$(ROOT_DIR)"/build/html

	@echo $(line_header)
	@echo "Completed HTML generation for GitHub repository branches and/or tags, next run 'make view-html'"
	@echo $(line_header)

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
	@echo "Completed HTML text generation, run 'make view-html'"
