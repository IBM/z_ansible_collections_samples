.. ...........................................................................
.. © Copyright IBM Corporation 2020                                          .
.. ...........................................................................

Features
========
BetterArgParser is a parser designed to validate values often used on z/OS to
avoid unnecessary failures on the target such as incorrectly providing a
data set name or type. It acts as an alternative to parsers such as argparse
and includes the following features:

* Arguments dependencies on other arguments
* Multiple levels of nested arguments (dict of dict, list of dict, dict of
  lists, etc.)
* Type checking and modification
* Default value handling
* Required arguments
* Providing list of valid choices
* Argument aliases
* Custom functions for type checking, defaults, and required arguments.
* Can leverage contents of arguments specified as dependencies.

Importing BetterArgParser
=========================

If the ``better_arg_parser.py`` is present in the *module_utils/* directory, you
can import it in the imports section of module with this example:

.. code-block:: python

   from ansible.module_utils.better_arg_parser import BetterArgParser

Defining Arguments
==================

Arguments need to be defined to the parser before the parser can begin
processing argument contents.

The parser expects to receive a Python dictionary where:

* key = the name of the argument
* value = a second dictionary containing configuration options for the argument

Argument Options
================

+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| Name               | Type            | Description                                                                                                              | Required                                                           | Default | Options                                                        |
+====================+=================+==========================================================================================================================+====================================================================+=========+================================================================+
| arg_type           | str or function | The expected type of the argument.                                                                                       | False                                                              | 'str'   | 'str', 'int', 'bool', 'list', 'dict', or user defined function |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| required           | bool            | Determines if the parameter must be provided. If default is set, required is unnecessary.                                | False                                                              | False   | True, False, or user defined function                          |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| default            | any             | The default value to be used when none is provided.                                                                      | False                                                              | None    | any, including user defined function                           |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| aliases            | list[str]       | A list of any alternative names to be used for the argument.                                                             | False                                                              | []      | N/A                                                            |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| choices            | list            | A list of choices accepted by the argument.                                                                              | False                                                              | N/A     | N/A                                                            |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| elements           | str             | When arg_type='list', elements specifies the expected type of the items in the list.                                     | True when arg_type='list'                                          | N/A     | 'str', 'int', 'bool', 'list', 'dict', or user defined function |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| options            | dict            | When arg_type='dict' or (arg_type='list' and elements='dict'), options is used to define a nested set of arguments.      | True when arg_type='dict' or (arg_type='list' and elements='dict') | N/A     | N/A                                                            |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| dependencies       | list[str]       | A list containing any other arguments needed during parsing of the current argument.                                     | False                                                              | []      | N/A                                                            |
|                    |                 | Any dependent arguments will be parsed first and their contents passed to any custom functions                           |                                                                    |         |                                                                |
|                    |                 | provided for arg_type, required, and default. Dependencies can only exist for arguments of the same depth.               |                                                                    |         |                                                                |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+
| mutually_exclusive | list[list[str]] | A list containing lists of mutually exclusive arguments. If two or more mutually exclusive arguments are                 | False                                                              | []      | N/A                                                            |
|                    |                 | provided at runtime, the parser will raise an exception. This should be placed  at the same level as                     |                                                                    |         |                                                                |
|                    |                 | arguments, not inside of an arguments option list.                                                                       |                                                                    |         |                                                                |
+--------------------+-----------------+--------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------+---------+----------------------------------------------------------------+

Example argument definition:

.. code-block:: python

   # argument definition dictionary
   arg_defs=dict(
       # argument depth = 1

       # first argument named "person"
       person=dict(
           # "person" contains a nested dictionary
           # containing more arguments
           arg_type='dict',
           options=dict(
               # argument depth = 2

               # if no "name" provided,
               # "name" will be set to "testname"
               name=dict(
                   arg_type='str',
                   default='testname'
               ),
               # if "age" not set that is fine
               age=dict(
                   arg_type='int',
                   required=False
               ),
               # "address" is another nested dict of arguments
               address=dict(
                   arg_type='dict',
                   options=dict(
                       # argument depth = 3

                       street=dict(
                           arg_type='str'
                       ),
                       number=dict(
                           arg_type='int'
                       ),
                       city=dict(
                           arg_type='str',
                           default='San Jose'
                       )
                   )
               )
           )
       )
   )

This argument definition would be used as in the following example:

.. code-block:: python

   parser = BetterArgParser(arg_defs)
   result = parser.parse_args({
       'person': {
           'name': 'blake',
           'age': 23,
           'address': {
               'street': 'bailey ave',
               'number': 555
           }
       }
   })


The contents of result for the example:

.. code-block:: python

   {
       'person': {
           'name': 'blake',
           'age': 23,
           'address': {
               'street': 'bailey ave',
               'number': 555,
               'city': 'San Jose'
           }
       }
   }

Built-in Argument Types (*arg_type* types)
==========================================

+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| Name              | Description                                                                                                                 | Example(s)                                                                                                         |
+===================+=============================================================================================================================+====================================================================================================================+
| dict              | An argument container type. Expects to be provided an `options argument`_.                                                  | N/A / Complex                                                                                                      |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| list              | An argument container type. Either expects to be provided an `elements argument`_                                           | N/A / Complex                                                                                                      |
|                   | where the element type is one of the built-in argument types or a `custom function type`_.                                  |                                                                                                                    |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| str               | Expects to receive a string.                                                                                                | `"Hello World"`                                                                                                    |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| bool              | Expects to receive a boolean.                                                                                               | `True` or `False`                                                                                                  |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| int               | Expects to receive an integer or a string containing only numeric characters. Returns an integer.                           | `12345`, `"12345"`                                                                                                 |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| path              | Expects to receive a valid absolute filesystem path. Only format is checked, provided path is not checked for existence.    | `"/usr/lpp/rsusr/bin/ported"`                                                                                      |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| data_set          | Expects to receive a valid data set name, with or without a PDS member. Case insensitive.                                   | `"USER.PRIVATE.PROCLIB"`, `"user.private.proclib"`, `"user.private.jobs(runthing)"`                                |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| data_set_base     | Expects to receive a valid data set name without a PDS member. Case insensitive.                                            | `"USER.PRIVATE.PROCLIB"`, `"user.private.proclib"`                                                                 |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| data_set_member   | Expects to receive a valid data set name with PDS member. Case insensitive.                                                 | `"user.private.jobs(runthing)"`                                                                                    |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| data_set_or_path  | A combination of *data_set* and *path* argument types. Case insensitive.                                                    | `"/usr/lpp/rsusr/bin/ported"`, `"USER.PRIVATE.PROCLIB"`, `"user.private.proclib"`, `"user.private.jobs(runthing)"` |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| qualifier         | Expects to receive ONE valid qualifier for a data set name.                                                                 | `"USER"`, `"PRIVATE"`, `"procl8b"`                                                                                 |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| qualifier_pattern | Expects to receive ONE valid qualifier search pattern (ends with `*`) for a data set name.                                  | `"US*"`, `"PRIVAT*"`                                                                                               |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| volume            | Expects to receive a valid volume serial.                                                                                   | `"000000"`, `"SCR013"`                                                                                             |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+
| encoding          | Expects not to receive any characters which are not present in currently available encoding types.                          |                                                                                                                    |
|                   | Does not verify encoding type actually exists.                                                                              | `"UTF-8"`, `"IBM-1047"`                                                                                            |
+-------------------+-----------------------------------------------------------------------------------------------------------------------------+--------------------------------------------------------------------------------------------------------------------+

.. _options argument:
   better_arg_parser.html#argument-options

.. _elements argument:
   better_arg_parser.html#argument-options

.. _custom function type:
   better_arg_parser.html#custom-function-arg-type

Custom Functions
================

In situations where complex operations have to be performed, defining a custom
function is recommended. Custom functions can be used in place of other options
when defining an argument. Custom functions must meet different requirements
depending on their usage.

**Requirements for All Custom Functions**

The function must have two parameters:

 #. **Parameter 1** - The contents of the argument currently being parsed
 #. **Parameter 2** - A dictionary containing resolved dependencies specified
    during argument definition
 #. **Parameter 3 (optional)** - A dictionary containing any custom arguments
    defined. See `custom argument options`_.

.. _custom argument options:
   better_arg_parser.html#id1


Both arguments 1 and 2 are required regardless of whether or not they are used.

Custom Function: arg_type
-------------------------

**Raises:** If the argument is to be considered invalid, the function should
raise a *ValueError* exception. This will halt program execution.

**Returns:** The function should return the contents of the argument after any
necessary operations and validation.

Custom Function: required
-------------------------

**Returns:** The function should return **True** or **False** depending on whether
the argument is required.

Custom Function: default
------------------------

**Returns:** The updated contents of the argument with any default value, if desired.

.. code-block:: python

   def uppercase_string(contents, dependencies):
       if not isinstance(contents, str):
           raise ValueError('The provided contents {} are not a valid string type.'.format(contents))
       return contents.upper()

   arg_defs=dict(

       name=dict(
           arg_type=uppercase_string,
           required=True
       )
   )

   parser = BetterArgParser(arg_defs)
   result = parser.parse_args({
       'name': 'blake'
   })

.. code-block:: python

   {
       'name': 'BLAKE'
   }

Custom Argument Options
=======================

BetterArgParser is designed to allow user provided argument options as long as the new option name does not overlap with any `reserved option names`_.

.. _reserved option names:
   better_arg_parser.html#argument-options

Custom options are meant to be used with `custom functions`_.

.. _custom functions:
    better_arg_parser.html#custom-functions

Custom Argument Option Example:

.. code-block:: python

   def special_names_get_uppercase(value, dependencies, kwargs):
       if value in kwargs.get("special_names"):
           return value.upper()
       return value

   arg_defs = dict(
       name=dict(
           arg_type=special_names_get_uppercase,
           required=True,
           default="samplename",
           special_names=["blake", "demetri", "ping", "crystal", "asif", "luke"],
       ),
   )
   parser = BetterArgParser(arg_defs)
   result = parser.parse_args({"name": "blake"})

The variable **result** would contain:

.. code-block:: python

   {
       'name': 'BLAKE'
   }

Mutually Exclusive Arguments
============================

BetterArgParser supports providing lists of mutually exclusive arguments.

Mutually Exclusive Arguments Example:

.. code-block:: python

   arg_defs = dict(
       name=dict(arg_type="str", required=True, default="samplename"),
       date=dict(arg_type="str", default="may 1, 2020"),
       time=dict(arg_type="int", default="3945297"),
       weather=dict(arg_type="str"),
       raining=dict(arg_type="bool"),
       mutually_exclusive=[["date", "time"], ["weather", "raining"]],
   )
   parser = BetterArgParser(arg_defs)

   parser.parse_args({"date": "tuesday", "time": 5000})

The above snippet would raise a *ValueError* exception because both the
*date* and *time* arguments were provided.

Dependent Arguments
===================

In complex cases, arguments may need to take the contents of other arguments
into account during parsing.

For example,

* A parameter may only be required if a different argument has a particular
  value.
  * e.g. Only require *doctor_appointment_date* if *has_illness=True*
* Default values can change depending on value of another argument
  * e.g. If *country=US* default for *currency_type* is *$*, if *country=UK*
  default *currency_type* is *£*

The *dependencies* argument option is used to specify the dependencies of an
argument.
Dependencies will always be resolved before the argument that depends on them.

The resolved dependencies are passed to any custom functions defined in the
argument which has the dependencies.

**Note:** Dependencies can only exist between arguments at the same depth.
In addition, dependencies cannot exist between elements in a list of dicts.

Dependencies Example
====================

.. code-block:: python

   def currency_symbol(value, dependencies):
       if dependencies.get('country') == 'us':
           return '$'
       if dependencies.get('country') == 'uk':
           return '£'
       return '?'

   arg_defs=dict(
       country=dict(
           arg_type='str',
           choices=['us', 'uk']
       ),
       currency_symbol=dict(
           arg_type='str',
           default=currency_symbol,
           dependencies=['country']
       )
   )

   parser = BetterArgParser(arg_defs)
   result = parser.parse_args({
       'country': 'us'
   })


The variable *result* would contain:

.. code-block:: python

   {
    'country': 'us',
    'currency_symbol': '$'
   }
