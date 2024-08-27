#!/bin/sh
###############################################################################
# © Copyright IBM Corporation 2024
###############################################################################

# Jinja template can access playbook vars and Ansible magic vars .
echo "Hello {{ ansible_user }}."
echo "The playbook was run from directory: {{ playbook_dir }}."
echo "The number is set to: {{ some_num }}".
echo ""

# Jinja template can implement conditionals.
{% if use_custom_msg %}
msg="{{ custom_msg }}"
{% else %}
msg="Default Hello World"
{% endif %}

# Jinja template can loop over a range.
{% for i in range(1, 1+some_num) %}
echo "{{ i }} - $msg"
{% endfor %}


echo ""
echo "My favorite programming languages in reverse alphabetical order are:"

# Jinja template can loop over a list.
{% for lang in fav_programming_languages | sort(reverse=True) %}
echo {{ lang }}
{% endfor %}
