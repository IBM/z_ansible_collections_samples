# -*- coding: utf-8 -*-
# Copyright (c) IBM Corporation 2020, 2025
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#     http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


from __future__ import absolute_import, division, print_function

__metaclass__ = type

from collections import OrderedDict, defaultdict
import types
from os import path
import sys
from re import IGNORECASE

if sys.version_info >= (3, 0):
    from inspect import getfullargspec
    from re import fullmatch
else:
    from inspect import getargspec as getfullargspec
    from re import match as fullmatch

# TODO: add "allow empty" parameter for each argument

DUMMY_ARG_NAME = "argholder"


class BetterArg(object):
    def __init__(
        self,
        arg_parser,
        name,
        elements=None,
        options=None,
        aliases=None,
        dependencies=None,
        required=False,
        default=None,
        choices=None,
        mutually_exclusive=None,
        arg_type="str",
        type=None,
        **kwargs
    ):
        """Holds all of the attributes that define a particular argument.
        A BetterArg object can contain nested BetterArg objects.

        Parameters
        ----------
        object : object
            The most base class type.
        arg_parser : BetterArgParser
            The instance of BetterArgParser
            used to create the BetterArg.
            Used to call BetterArgParser.handle_args() when nested BetterArg
            objects need to be defined.
        name : str
            The name of the argument to define.

        Keyword Parameters
        ------------------
        elements : Union[str, function]
            Used to specify the expected
            type for each list element when the arg_type is 'list'. (default: {None})
        options : dict
            When arg_type or elements = 'dict', a dictionary
            containing details for a nested group of arguments should be
            provided. (default: {None})
        aliases : list[str]
            A list of alternative names that can be
            used to refer to the argument. (default: {[]})
        dependencies : list
            A list of arguments that should be resolved
            before parsing this argument. (default: {[]})
        required : Union[bool, function]
            Determines if later parsing
            should fail when no value is provided for the argument. Not necessary if
            default is provided. (default: {False})
        default : Union[str, int, bool, function]
            The default value that the
            argument should be set to when none is provided. (default: {None})
        choices : list[Union[str, int, bool]]
            The list of valid contents for the argument.
        mutually_exclusive : list[list[str]]
            A list containing lists of mutually exclusive argument names.
            (default: {None})
        arg_type : Union[str, function]
            The type the argument contents should be. (default: {'str'})
        type : Union[str, function]
            The type the argument contents should be. Alternative to arg_type.
            (default: {'str'})
        """
        if aliases is None:
            aliases = []
        if dependencies is None:
            dependencies = []
        if choices is None:
            choices = []
        if mutually_exclusive is None:
            mutually_exclusive = []
        self.arg_parser = arg_parser
        self.name = name
        self.elements = elements
        self.options = None
        self.aliases = aliases
        self.dependencies = dependencies
        self.required = required
        self.default = default
        self.choices = choices
        self.arg_type = type if type is not None else arg_type
        if options:
            self.options = self.arg_parser.handle_args(options)
            self.mutually_exclusive = self.arg_parser.handle_mutually_exclusive_args(
                mutually_exclusive
            )
        self.kwargs = kwargs


class BetterArgHandler(object):
    def __init__(self, arg_name, contents, resolved_args, arg_defs):
        """Sets, formats and validates an argument and its contents based on its
        matching BetterArg object.

        Parameters
        ----------
        arg_name : str
            The name of the argument.
        contents : dict
            The argument contents to be handled by the
            argument's BetterArg object
        resolved_args : dict
            Contains all of the dependencies and their
            contents, which have already been handled by a BetterArgHandler, for use
            during current arguments handling operations.
        arg_defs : dict[str, BetterArg]
            All of the BetterArg argument
            definitions for current argument depth.
        """
        self.arg_name = arg_name
        self.arg_defs = arg_defs
        self.arg_def = arg_defs.get(arg_name)
        self.contents = contents
        self.resolved_dependencies = self.build_resolved_dependency_dict(resolved_args)
        # TODO: determine if we should optionally allow top-level args to be passed
        self.type_handlers = {
            "basic_dict": self._basic_dict_type,
            "dict": self._dict_type,
            "list": self._list_type,
            "str": self._str_type,
            "bool": self._bool_type,
            "int": self._int_type,
            "signed_int": self._signed_int_type,
            "path": self._path_type,
            "path_or_empty": self._path_or_empty_type,
            "data_set": self._data_set_type,
            "data_set_base": self._data_set_base_type,
            "data_set_member": self._data_set_member_type,
            "member_name": self._member_name_type,
            "identifier_name": self._identifier_name_type,
            "qualifier": self._qualifier_type,
            "qualifier_or_empty": self._qualifier_or_empty_type,
            "qualifier_pattern": self._qualifier_pattern_type,
            "username_pattern": self._username_pattern_type,
            "volume": self._volume_type,
            "data_set_or_path": self._data_set_or_path_type,
            "encoding": self._encoding_type,
            "dd": self._dd_type,
            "job_identifier": self._job_identifier,
        }

    def _basic_dict_type(self, contents, resolve_dependencies):
        """Resolver for basic dict type arguments.
        Parameters
        ----------
        contents : dict
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.
        Returns
        -------
        dict
            The arguments contents after any necessary operations.
        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not isinstance(contents, dict):
            raise ValueError('Invalid argument "{0}" for type "dict".'.format(contents))
        return contents

    def handle_arg(self):
        """Performs all setting, formatting and validation operations for a single argument.

        Returns
        -------
        dict
            The arguments contents after any necessary operations.
        """

        self._resolve_required()
        self.contents = self._resolve_default()
        if self.contents is not None:
            # ? is this the best way to handle this?
            self.contents = self._resolve_arg_type()
            self._resolve_choices()
        return self.contents

    def _list_type(self, contents, resolved_dependencies):
        """Resolver for list type arguments.

        Parameters
        ----------
        contents : list[Union[int, str, bool, dict]]
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        Union[list[int, str, bool, dict]]
            The arguments contents after any necessary operations.
        """
        # TODO: determine how to handle resolved dependencies for list items, probably good as-is
        updated_contents = []
        if BetterArgHandler.is_function(self.arg_def.elements):
            for item in contents:
                updated_contents.append(
                    self._call_arg_function(self.arg_def.elements, item)
                )

        elif self.type_handlers.get(self.arg_def.elements):
            for item in contents:
                updated_contents.append(
                    self.type_handlers.get(self.arg_def.elements)(
                        item, self.resolved_dependencies
                    )
                )
        contents = updated_contents
        return contents

    def _dict_type(self, contents, resolved_dependencies):
        """Resolver for dict type arguments.

        Parameters
        ----------
        contents : dict
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        dict
            The arguments contents after any necessary operations.
        """
        updated_contents = {}
        # for key, value in contents.items():
        for key in self.arg_def.options:
            handler = BetterArgHandler(
                key, contents.get(key), updated_contents, self.arg_def.options
            )
            updated_value = handler.handle_arg()
            updated_contents[key] = updated_value
        contents = updated_contents
        self._assert_mutually_exclusive(contents)
        return contents

    def _str_type(self, contents, resolve_dependencies):
        """Resolver for str type arguments.

        Parameters
        ----------
        contents : str
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not isinstance(contents, str):
            raise ValueError('Invalid argument "{0}" for type "str".'.format(contents))
        return contents

    def _int_type(self, contents, resolve_dependencies):
        """Resolver for int type arguments.

        Parameters
        ----------
        contents : Union[int, str]
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        int
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(r"[0-9]+", str(contents)):
            raise ValueError('Invalid argument "{0}" for type "int".'.format(contents))
        return int(contents)

    def _signed_int_type(self, contents, resolve_dependencies):
        """Resolver for signed int type arguments.

        Parameters
        ----------
        contents : Union[int, str]
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        int
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(r"[-+]?[0-9]+", str(contents)):
            raise ValueError('Invalid argument "{0}" for type "signed_int".'.format(contents))
        return int(contents)

    def _bool_type(self, contents, resolve_dependencies):
        """Resolver for bool type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        bool
            The arguments contents after any necessary operations.

        Raises
        ------
            ValueError: When contents is invalid argument type.
        """
        if not isinstance(contents, bool):
            raise ValueError('Invalid argument "{0}" for type "bool".'.format(contents))
        return contents

    def _member_name_type(self, contents, resolve_dependencies):
        """Resolver for PDS/E member name type arguments. This is part of
           zos_started_task member name validation.
        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.
        Returns
        -------
        str
            The arguments contents after any necessary operations.
        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^[A-Z$#@]{1}[A-Z0-9$#@]{0,7}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "member_name".'.format(contents)
            )
        return str(contents)

    def _identifier_name_type(self, contents, resolve_dependencies):
        """Resolver for identifier name type arguments. This is part of
           zos_started_task identifier name validation.
        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.
        Returns
        -------
        str
            The arguments contents after any necessary operations.
        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^[A-Z]{1}[A-Z0-9$#@]{0,7}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "identifier_name".'.format(contents)
            )
        return str(contents)

    def _path_type(self, contents, resolve_dependencies):
        """Resolver for path type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type
        """
        contents = BetterArgHandler.fix_local_path(contents)
        if not path.isabs(str(contents)):
            raise ValueError('Invalid argument "{0}" for type "path".'.format(contents))
        return str(contents)

    def _path_or_empty_type(self, contents, resolve_dependencies):
        """Resolver for path or empty string type arguments.

        Parameters
        ----------
        contents : str
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type
        """
        contents = BetterArgHandler.fix_local_path(contents)
        if contents == "":
            return contents
        if not path.isabs(str(contents)):
            raise ValueError('Invalid argument "{0}" for type "path_or_empty".'.format(contents))
        return str(contents)

    # ---------------------------------------------------------------------------- #
    #                             DATA SET NAMING RULES                            #
    # ---------------------------------------------------------------------------- #
    # A data set name consists of one or more parts connected by periods. Each part is called a qualifier.
    # Each qualifier must begin with an alphabetic character (A-Z) or the special characters $, #, @.
    # The remaining characters in each qualifier can be alphabetic characters, digits (0-9), a hyphen (-),
    #  or the special characters $, #, @.
    # Each qualifier must be one to eight characters long.
    # The maximum length of a complete data set name before specifying a member name is 44 characters,
    # including the periods.

    # ---------------------------------------------------------------------------- #
    #                            PDS member naming rules                           #
    # ---------------------------------------------------------------------------- #
    # A member name cannot be longer than eight characters.
    # The first member character must be either a letter or one of the following three special characters: #, @, $.
    # The remaining seven characters can be letters, numbers, or one of the following special characters: #, @, or $.
    # A PDS member name cannot contain a hyphen (-).
    # A PDS member name cannot contain accented characters (à, é, è, and so on).

    def _data_set_type(self, contents, resolve_dependencies):
        """Resolver for data_set type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}(?:\([A-Z$#@]{1}[A-Z0-9$#@]{0,7}\)|\(([-+]?[0-9]+)\)){0,1}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "data_set".'.format(contents)
            )
        return str(contents)

    def _data_set_base_type(self, contents, resolve_dependencies):
        """Resolver for data_set_base type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "data_set_base".'.format(contents)
            )
        return str(contents)

    def _data_set_member_type(self, contents, resolve_dependencies):
        """Resolver for data_set_member type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}\([A-Z$#@]{1}[A-Z0-9$#@]{0,7}\)$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "data_set_member".'.format(contents)
            )
        return str(contents)

    def _qualifier_type(self, contents, resolve_dependencies):
        """Resolver for qualifier type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises:
            ValueError: When contents is invalid argument type.
        """
        if not fullmatch(
            r"^[A-Z]{1}[A-Z0-9]{0,7}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "qualifier".'.format(contents)
            )
        return str(contents)

    def _qualifier_or_empty_type(self, contents, resolve_dependencies):
        """Resolver for qualifier type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^[A-Z]{1}[A-Z0-9]{0,7}$",
            str(contents),
            IGNORECASE,
        ) and str(contents) != "":
            raise ValueError(
                'Invalid argument "{0}" for type "qualifier".'.format(contents)
            )
        return str(contents)

    def _qualifier_pattern_type(self, contents, resolve_dependencies):
        """Resolver for qualifier_pattern type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^(?:[A-Z]{1}[A-Z0-9]{0,7})|(?:\*{1})|(?:[A-Z]{1}[A-Z0-9]{0,6}\*{1})$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "qualifier_pattern".'.format(contents)
            )
        return str(contents)

    def _username_pattern_type(self, contents, resolve_dependencies):
        """Resolver for username_pattern type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        # Valid characters are the following:
        # A - Z, 0 - 9, $, @, #
        if not fullmatch(
            r"^(?:[A-Z$#@]{1}[A-Z0-9$#@]{0,7})|(?:\*{1})|(?:[A-Z$#@]{1}[A-Z0-9$#@]{0,6}\*{1})$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument type for "{0}". Expected a valid username.'.format(
                    contents
                )
            )
        return str(contents)

    def _volume_type(self, contents, resolve_dependencies):
        """Resolver for volume type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^[A-Z0-9@#$]{1,6}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "volume".'.format(contents)
            )
        return str(contents)

    def _dd_type(self, contents, resolve_dependencies):
        """Resolver for dd type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"^[A-Z$#@][A-Z0-9@#$]{0,7}$",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError('Invalid argument "{0}" for type "dd".'.format(contents))
        return str(contents)

    @staticmethod
    def fix_local_path(given_path):
        """Adapter for local/USS path abbreviations.

        Parameters
        ----------
        given_path : str
            Path given as input, which may need adjustment.

        Returns
        -------
        str
            The path, after leading ~, .. or . has been adjusted.
        """
        final_path = given_path
        if given_path.startswith("~"):
            final_path = path.expanduser(given_path)
        elif given_path.startswith(".."):
            pwd = str(path.realpath(".."))
            final_path = pwd + given_path[2:]
        elif given_path.startswith("."):
            wd = str(path.realpath("."))
            final_path = wd + given_path[1:]

        return str(final_path)

    def _data_set_or_path_type(self, contents, resolve_dependencies):
        """Resolver for data_set_or_path type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            #  HLQ and all middle level qualifiers.                Last qualifier bef members.    Normal members.                 GDS members.
            r"^(?:(?:[A-Z$#@]{1}[A-Z0-9$#@-]{0,7})(?:[.]{1})){1,21}[A-Z$#@]{1}[A-Z0-9$#@-]{0,7}(?:\([A-Z$#@]{1}[A-Z0-9$#@]{0,7}\)|\(([-+]?[0-9]+)\)){0,1}$",
            str(contents),
            IGNORECASE,
        ):
            content_path = str(contents)
            contents = BetterArgHandler.fix_local_path(content_path)

            if not path.isabs(contents):
                raise ValueError(
                    'Invalid argument "{0}" for type "data_set" or "path".'.format(
                        contents
                    )
                )
        return str(contents)

    def _encoding_type(self, contents, resolve_dependencies):
        """Resolver for encoding type arguments.

        Parameters
        ----------
        contents : bool
            The contents of the argument.
        resolved_dependencies : dict
            Contains all of the dependencies and their contents,
            which have already been handled,
            for use during current arguments handling operations.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(r"^[A-Z0-9-]{2,}$", str(contents), IGNORECASE):
            raise ValueError(
                'Invalid argument "{0}" for type "encoding".'.format(contents)
            )
        return str(contents)

    @staticmethod
    def is_function(some_var):
        """Determines if variable is a function.

        Parameters
        ----------
        some_var : Union[str, int, bool, function]
            The variable to test for type.

        Returns
        -------
        bool
            True if variable some_var is function, False otherwise.
        """
        return isinstance(some_var, types.FunctionType)

    def _resolve_required(self):
        """Perform operations to determine if an argument is required.

        Raises
        ------
        ValueError
            When no value or defaults are provided for a required argument.
        """
        required = self.arg_def.required
        if BetterArgHandler.is_function(self.arg_def.required):
            required = self._call_arg_function(self.arg_def.required, self.contents)
        if self.contents is None and required is True and self.arg_def.default is None:
            raise ValueError("Missing required argument {0}".format(self.arg_name))
        return

    # TODO: add additional skips when argument type should never use default even when set
    def _resolve_default(self):
        """Resolve the default value of an argument when no value is provided.

        Returns
        -------
        Union[str, int, bool, list, dict]
            The updated contents of the argument.
        """
        if self.contents is not None:
            return self.contents
        new_contents = None
        if BetterArgHandler.is_function(self.arg_def.default):
            new_contents = self._call_arg_function(self.arg_def.default, self.contents)
        else:
            new_contents = self.arg_def.default
        self.contents = new_contents
        return self.contents

    def _resolve_choices(self):
        """Verify the argument contents are a valid choice when list of choices is provided.

        Raises
        ------
        ValueError
            The provided value is not a valid choice.
        """
        if self.arg_def.choices and len(self.arg_def.choices) > 0:
            if self.contents not in self.arg_def.choices:
                raise ValueError(
                    "Provided value: {0} for arg: {1} is not in a valid choice. Choices: {2}".format(
                        self.contents, self.arg_name, ", ".join(self.arg_def.choices)
                    )
                )

    def _resolve_arg_type(self):
        """Perform type-specific operations for an argument.
        This may include manipulating argument contents and/or
        any necessary validation.

        Returns
        -------
        Union[str, int, bool, list, dict]
            The argument's contents after any necessary processing by type handler.

        Raises
        ------
        ValueError
            When the provided arg_type is invalid.
        """
        if BetterArgHandler.is_function(self.arg_def.arg_type):
            return self._call_arg_function(self.arg_def.arg_type, self.contents)
        elif self.type_handlers.get(self.arg_def.arg_type):
            return self.type_handlers.get(self.arg_def.arg_type)(
                self.contents, self.resolved_dependencies
            )
        else:
            raise ValueError(
                'Provided arg_type "{0}" for argument "{1}" is invalid.'.format(
                    self.arg_name, self.arg_def.arg_type
                )
            )

    # ? resolved_args should be a dict?
    def build_resolved_dependency_dict(self, resolved_args):
        """Gather all arguments for which the current argument has
        a dependency. These dependency arguments should already be
        resolved by the time this method is called.

        Parameters
        ----------
        resolved_args : dict
            Arguments that have already finished parser processing.

        Returns
        -------
        dict
            Subset of resolved_args containing any dependencies required by current argument.
        """
        resolved_dependencies = {}
        for dependency in self.arg_def.dependencies:
            resolved_dependencies[dependency] = resolved_args.get(dependency)
        return resolved_dependencies

    # TODO: reduce complexity of this function
    def _assert_mutually_exclusive(self, contents):
        """Assert none of the provided arguments
        break mutual exclusivity.

        Parameters
        ----------
        contents : dict
            Argument dict for level of arguments.
            Need to determine if any 2 or more of the arguments in the
            dict are breaking mutual exclusivity rules.

        Raises
        ------
        ValueError
            When two or more mutually exclusive arguments are found.
        """
        for name, name_list in self.arg_def.mutually_exclusive.items():
            arg = contents.get(name)
            if arg is None:
                continue
            for exclusive_name in name_list:
                if contents.get(exclusive_name) is not None:
                    raise ValueError(
                        "Mutually exclusive arguments {0} and {1} both have values.".format(
                            name, exclusive_name
                        )
                    )
        return

    def _num_of_params(self, arg_function):
        """Get the number of parameters accepted by a function.

        Parameters
        ----------
        arg_function : function
            The function to inspect.

        Returns
        -------
        int
            The number of parameters the function accepts.
        """
        spec = getfullargspec(arg_function)
        length = len(spec[0])
        if spec[1]:
            length += 1
        if spec[2]:
            length += 1
        return length

    def _call_arg_function(self, arg_function, contents):
        """Call a function with the correct number
        of arguments.

        Parameters
        ----------
        arg_function : function
            The function to call.
        contents : Union[str,list,dict,int,bool]
            The argument contents to pass to the function.

        Returns
        -------
            Any -- Returns the result of the function call.

        Raises
        ------
        ValueError
            When the provided function's number of parameters do not match BetterArgParser spec.
        """
        number_of_params = self._num_of_params(arg_function)
        if number_of_params == 2:
            return arg_function(contents, self.resolved_dependencies)
        elif number_of_params == 3:
            return arg_function(
                contents, self.resolved_dependencies, self.arg_def.kwargs
            )
        else:
            raise ValueError(
                "Provided function {0} for argument {1} has invalid number of parameters.".format(
                    arg_function, self.arg_name
                )
            )

    # ---------------------------------------------------------------------------- #
    #                    JOB ID AND JOB NAME NAMING RULES                          #
    # ---------------------------------------------------------------------------- #

    def _job_identifier(self, contents, resolve_dependencies):
        """Resolver for data_set type arguments.
        A text string of up to 8 characters.
        The first character must be a letter or a national (#, $, @) character.
        Other characters can be letters, numbers, or national (#, $, @) characters.
        If the text string contains #, $, or @, enclose the text string in single or double quotation marks.

        Parameters
        ----------
        contents : str
            The contents of the argument.

        Returns
        -------
        str
            The arguments contents after any necessary operations.

        Raises
        ------
        ValueError
            When contents is invalid argument type.
        """
        if not fullmatch(
            r"(^[a-zA-Z$#@%}]{1}[0-9a-zA-Z$#@%*]{1,7})|(^['\*']{1})",
            str(contents),
            IGNORECASE,
        ):
            raise ValueError(
                'Invalid argument "{0}" for type "job_id or job_name".'.format(contents)
            )
        return str(contents)


class BetterArgParser(object):
    def __init__(self, arg_dict):
        """Parser used to verify provided arguments
        match defined criteria, and perform any
        necessary operations on the provided arguments.

        Parameters
        ----------
        object : object
            The most base type
        arg_dict : dict[str, dict]
            A list of key:value pairs where key = argument name
            and value = BetterArg object
        """
        self.aliases = {}
        # self.args = self.handle_args(arg_dict)
        mutually_exclusive = arg_dict.get("mutually_exclusive")
        arg_dict.pop("mutually_exclusive", None)
        argholder = dict(
            arg_type="dict",
            mutually_exclusive=mutually_exclusive,
            options=arg_dict,
        )
        self.args = OrderedDict()
        self.args[DUMMY_ARG_NAME] = BetterArg(self, DUMMY_ARG_NAME, **argholder)

    def handle_args(self, arg_dict):
        """Handles argument definition operations.
        Builds dict of BetterArg objects based on argument
        definitions, swaps out alias names, and sorts and verifies no
        invalid or cyclic dependencies exist.

        Parameters
        ----------
        arg_dict : dict
            The argument definitions used to generate BetterArg objects.

        Returns
        --------
        OrderedDict[str, BetterArg]
            The defined arguments, sorted based on their dependencies.
        """
        args = {}
        for key, value in arg_dict.items():
            args[key] = BetterArg(self, key, **value)
            self.aliases = self._add_alias(key, value.get("aliases", []), self.aliases)

        args = self._swap_alias_for_real_names(args, self.aliases)
        self._assert_no_invalid_dependencies(args)
        args = self._sort_args_by_dependencies(args)
        return args

    def parse_args(self, arg_dict):
        """Parse provided argument values using corresponding
        BetterArg argument definition.

        Parameters
        ----------
        arg_dict : dict
            The arguments to parse where key=argument name/alias
            and value=argument contents.

        Returns
        -------
        dict
            The arguments with alias names swapped for real names
            and contents after any necessary operations and checks have been performed.
        """
        parsed_args = {}
        arg_dict = self._swap_alias_for_real_names(arg_dict, self.aliases)
        arg_dict_wrapper = {}
        arg_dict_wrapper[DUMMY_ARG_NAME] = arg_dict
        for key in self.args:
            handler = BetterArgHandler(
                key, arg_dict_wrapper.get(key), parsed_args, self.args
            )
            updated_value = handler.handle_arg()
            parsed_args[key] = updated_value

        parsed_args = updated_value
        return parsed_args

    def _add_alias(self, arg_name, arg_aliases=None, aliases=None):
        """Add alias to an alias dictionary that can be
        used to simplify alias->name determinations.

        Parameters
        ----------
        arg_name : str
            The name of the argument.

        Keyword Parameters
        ------------------
        arg_aliases : list[str]
            The list of aliases for the argument name (default: {None}).
        aliases : dict
            The dictionary containing all of the currently defined aliases. (default: {None}).

        Returns
        -------
        dict
            The updated dict of aliases.

        Raises
        ------
        ValueError
            When conflicting aliases are found.
        """
        if arg_aliases is None:
            arg_aliases = []
        if aliases is None:
            aliases = {}
        arg_aliases.append(arg_name)
        for alternate_name in arg_aliases:
            aliases[alternate_name] = arg_name
        return aliases

    def _swap_alias_for_real_names(self, args, aliases):
        """Swap any arguments provided with alias for a name
        with their 'real' name used to refer to the argument
        throughout parser.

        Parameters
        ----------
        args : dict
            Arguments for BetterArgParser to parse.
        aliases : dict
            The dictionary containing all of the currently defined aliases.

        Returns
        -------
        dict
            The contents from provided argument where they keys
            have been swapped for 'real' argument names where necessary.
        """
        renamed_args = {}
        for alias_name, value in args.items():
            renamed_args[aliases.get(alias_name)] = value
        args = renamed_args
        return args

    def handle_mutually_exclusive_args(self, mutually_exclusive):
        """Format mutually exclusive argument definitions. Into dictionary
        for simplified exclusivity checking.

        Parameters
        ----------
        mutually_exclusive : list[list[str]]
            List of lists containing mutually exclusive arguments.

        Returns
        -------
        dict
            Dict where key is the mutually exclusive
            argument name and value is a list of all arguments
            it is mutually exclusive with.
        """
        self._assert_mutually_exclusive_args_structure(mutually_exclusive)
        mutually_exclusive_param_dict = defaultdict(list)
        for exclusives in mutually_exclusive:
            for index, item in enumerate(exclusives):
                mutually_exclusive_param_dict[item] = mutually_exclusive_param_dict[
                    item
                ] + [x for i, x in enumerate(exclusives) if i != index]
        return mutually_exclusive_param_dict

    def _assert_mutually_exclusive_args_structure(self, mutually_exclusive):
        """Used to enforce structure of mutually_exclusive argument.

        Parameters
        ----------
        mutually_exclusive : list[list[str]]
            The mutually exclusive argument to validate.

        Raises
        ------
        ValueError
            When not in proper format.
        """
        try:
            if isinstance(mutually_exclusive, list):
                for exclusives in mutually_exclusive:
                    if isinstance(exclusives, list):
                        for item in exclusives:
                            if not isinstance(item, str):
                                raise ValueError
                    else:
                        raise ValueError
            elif mutually_exclusive is not None:
                raise ValueError
        except ValueError:
            raise ValueError(
                "Mutually exclusive arguments must be provided as a list of lists of strings."
            )
        return

    def _assert_no_invalid_dependencies(self, args):
        """Verify that no dependencies are requested
        that do not have an argument with matching name.

        Parameters
        ----------
        args : dict[str, BetterArg]
            All of the BetterArg argument definitions for current argument depth.

        Returns
        -------
        bool
            Always returns True when no invalid dependencies found.

        Raises
        ------
        ValueError
            When invalid dependency found.
        """
        valid_names = args.keys()
        dependencies = []
        for key, value in args.items():
            dependencies += value.dependencies
        bad_dependencies = list(set(dependencies) - set(valid_names))
        if bad_dependencies:
            raise ValueError(
                "One or more invalid dependencies found: {0}".format(
                    ", ".join(bad_dependencies)
                )
            )
        return True

    def _sort_args_by_dependencies(self, args):
        """Sort arguments based on their dependencies to other arguments.
        Used with _dependency_sort_helper() to implement topographical sorting.

        Parameters
        ----------
        args : dict[str, BetterArg]
            All of the BetterArg argument definitions for current argument depth.

        Returns
        -------
        OrderedDict[str, BetterArg]
            All of the BetterArg argument definitions for current argument depth,
            sorted based on dependencies.
        """
        visited = {name: False for name in args}
        dependencies = {}
        ordered_arg_defs = OrderedDict()
        for name in args:
            if not visited.get(name):
                self._dependency_sort_helper(
                    args, name, visited, dependencies, ordered_arg_defs
                )
        args = ordered_arg_defs
        return args

    def _dependency_sort_helper(
        self, args, name, visited, dependencies, ordered_arg_defs
    ):
        """Recursive helper function for _sort_args_by_dependencies().
        Used with _sort_args_by_dependencies() to implement topographical sorting.

        Parameters
        ----------
        args : dict[str, BetterArg]
            All of the BetterArg argument definitions for current argument depth.
        name : str
            The name of the argument
        visited : dict[str, bool]
            Holds the name of each argument in a key with
            a boolean value to identify whether operations have already been performed on the argument
        dependencies : dict[str, dict[str, bool]]
            Each outer key represents one argument where the
            value is a dictionary with key=name of argument outer key argument is dependent on. Boolean value
            is always true and is a placeholder.
        ordered_arg_defs : dict[str, BetterArg]
            Argument definitions
            from arg_defs sorted based on their dependencies,
            output is in the reverse of the order desired. Reverse sorting is handled in _sort_args_by_dependencies().

        Raises
        ------
        RuntimeError
            When cyclic dependencies are found
        """
        visited[name] = True
        dependencies[name] = {
            dep_name: True for dep_name in args.get(name).dependencies
        }
        # TODO: fix dependency cycles
        if self._has_cycle(args):
            raise RuntimeError("Cyclic dependency found.")
        for dependency_name in args.get(name).dependencies:
            if not visited.get(dependency_name):
                self._dependency_sort_helper(
                    args, dependency_name, visited, dependencies, ordered_arg_defs
                )
        ordered_arg_defs[name] = args.get(name)
        return

    def _has_cycle(self, args):
        """Determines if cyclic dependencies exist between arguments.

        Parameters
        ----------
        args : dict[str, BetterArg]
            All of the BetterArg argument definitions for current argument depth.

        Returns
        -------
        bool
            True if cycle exists False otherwise.
        """
        graph = defaultdict(list)
        arg_name_to_num = {}
        for i, arg in enumerate(args):
            arg_name_to_num[arg] = i
        visited = [False for x in range(len(arg_name_to_num))]
        stack = [False for x in range(len(arg_name_to_num))]
        for name, value in args.items():
            for dependency_name in value.dependencies:
                graph[arg_name_to_num.get(name)].append(
                    arg_name_to_num.get(dependency_name)
                )
        for i in arg_name_to_num.values():
            if not visited[i]:
                if self._is_cyclic_helper(i, visited, stack, graph):
                    return True
        return False

    def _is_cyclic_helper(self, i, visited, stack, graph):
        """Works with _has_cycle() to determine if cyclic dependencies exist between arguments.

        Parameters
        ----------
        i : int
            The index for the current argument.
        visited : list[bool]
            Maintains a record of which arguments have been visited.
        stack : list[bool]
            Used with visited to identify cycles.
        graph : defaultdict[list]
            The graph representing our argument dependencies as numbers.

        Returns
        -------
        bool
            True if cycle exists, False otherwise.
        """
        visited[i] = True
        stack[i] = True
        for neighbor in graph[i]:
            if visited[neighbor] is False:
                if self._is_cyclic_helper(neighbor, visited, stack, graph):
                    return True
            elif stack[neighbor] is True:
                return True
        stack[i] = False
        return False
