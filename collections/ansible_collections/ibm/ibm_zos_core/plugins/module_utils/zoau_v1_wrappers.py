from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.import_handler import (
    MissingZOAUImport,
)

try:
    ZOAU_API_VERSION = 2
    # try to import ZOAU >= 1.1.0
    from zoautil_py import datasets, mvscmd, types
    from zoautil_py.exceptions import ZOAUException
except ImportError:
    ZOAU_API_VERSION = 1
    try:
        # try to import ZOAU < 1.1.0
        from zoautil_py import Datasets as datasets
        from zoautil_py import MVSCmd as mvscmd
        from zoautil_py import types

        class ZOAUException(Exception):
            pass

    except ImportError:
        Datasets = MissingZOAUImport()
        MVSCmd = MissingZOAUImport()
        types = MissingZOAUImport()


class Datasets(object):
    @staticmethod
    def read(dataset, *args, **kwargs):
        contents = ""
        if ZOAU_API_VERSION == 1:
            contents = datasets.read(dataset)
        elif ZOAU_API_VERSION == 2:
            try:
                contents = datasets.read(dataset, *args, **kwargs)
            except ZOAUException:
                if not kwargs.get("ignore_exceptions"):
                    raise
        return contents

    @staticmethod
    def read_last(dataset, lines=10, *args, **kwargs):
        contents = ""
        if ZOAU_API_VERSION == 1:
            contents = datasets.read_last(dataset, lines)
        elif ZOAU_API_VERSION == 2:
            try:
                kwargs["tail"] = lines
                contents = datasets.read(dataset, *args, **kwargs)
            except ZOAUException:
                if not kwargs.get("ignore_exceptions"):
                    raise
        return contents

    @staticmethod
    def read_from(dataset, start=1, *args, **kwargs):
        contents = ""
        if ZOAU_API_VERSION == 1:
            contents = datasets.read_from(dataset, start)
        elif ZOAU_API_VERSION == 2:
            try:
                kwargs["from_line"] = start
                contents = datasets.read(dataset, *args, **kwargs)
            except ZOAUException:
                if not kwargs.get("ignore_exceptions"):
                    raise
        return contents

    @staticmethod
    def write(dataset, content, append=False, *args, **kwargs):
        contents = ""
        if ZOAU_API_VERSION == 1:
            contents = datasets.write(dataset, content, append)
        elif ZOAU_API_VERSION == 2:
            try:
                contents = datasets.write(dataset, content, append, *args, **kwargs)
            except ZOAUException:
                if not kwargs.get("ignore_exceptions"):
                    raise
        return contents

    @staticmethod
    def create(name, type, *args, **kwargs):
        v1_args = ["size", "format", "class_name", "length", "offset"]
        for index, arg in enumerate(args):
            kwargs[v1_args[index]] = arg
        contents = ""
        if ZOAU_API_VERSION == 1:
            v1_args_and_kwargs = [
                "size",
                "format",
                "class_name",
                "length",
                "offset",
                "secondary_space",
                "directory_blocks",
                "key_offset",
                "key_length",
                "data_class",
                "management_class",
                "block_size",
                "volumes",
            ]
            final_kwargs = {
                key: value for key, value in kwargs.items() if key in v1_args_and_kwargs
            }
            contents = datasets.create(name, type, **final_kwargs)
        elif ZOAU_API_VERSION == 2:
            v2_arg_and_kwarg_dict = {
                "primary_space": "primary_space",
                "secondary_space": "secondary_space",
                "block_size": "block_size",
                "record_format": "record_format",
                "storage_class_name": "storage_class_name",
                "data_class_name": "data_class_name",
                "management_class_name": "management_class_name",
                "record_length": "record_length",
                "key_length": "key_length",
                "key_offset": "key_offset",
                "directory_blocks": "directory_blocks",
                "volumes": "volumes",
                "class_name": "storage_class_name",
                "length": "record_length",
                "offset": "key_offset",
            }
            try:
                final_kwargs = {}
                for key, value in kwargs.items():
                    if v2_arg_and_kwarg_dict.get(key):
                        final_kwargs[v2_arg_and_kwarg_dict.get(key)] = value
                contents = datasets.create(name, type, **final_kwargs)
            except ZOAUException:
                if not kwargs.get("ignore_exceptions"):
                    raise
        return contents
