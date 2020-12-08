#!/usr/bin/env python3
# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

import re
import datetime

def _process_output(stdout):
    """Process output of LISTVTOC.

    Arguments:
        stdout {str} -- The output of LISTVTOC.

    Returns:
        list[dict] -- List of dictionaries holding data set information from VTOC.
    """
    data_sets = []
    data_set_strings = _separate_data_set_sections(stdout)
    for data_set_string in data_set_strings:
        data_sets.append(_parse_data_set_info(data_set_string))
    return data_sets


def _separate_data_set_sections(contents):
    """Split LISTVTOC output into data set sections.

    Arguments:
        contents {str} -- The output of LISTVTOC.

    Returns:
        list[str] -- LISTVTOC output separated into sections by data set.
    """
    delimeter = "0---------------DATA SET NAME----------------"
    data_sets = re.split(delimeter, contents)
    fixed_ds = [delimeter + x for x in data_sets[1:]]
    return fixed_ds


def _parse_data_set_info(data_set_string):
    """Build dictionaries representing data set information
    from LISTVTOC output.

    Arguments:
        data_set_string {str} -- Single data set section of the LISTVTOC output.

    Returns:
        dict -- Holds data set information from VTOC.
    """
    lines = data_set_string.split("\n")
    data_set_info = {}
    regex_for_rows = [
        (
            r"(0-*DATA SET NAME-*\s+)(SER NO\s+)(SEQNO\s+)(DATE.CRE\s+)(DATE.EXP\s+)"
            r"(DATE.REF\s+)(EXT\s+)(DSORG\s+)(RECFM\s+)(OPTCD\s+)(BLKSIZE[ ]*)"
        ),
        (
            r"(0SMS.IND\s+)(LRECL\s+)(KEYLEN\s+)(INITIAL ALLOC\s+)(2ND ALLOC\s+)"
            r"(EXTEND\s+)(LAST BLK\(T-R-L\)\s+)(DIR.REM\s+)(F2 OR F3\(C-H-R\)\s+)(DSCB\(C-H-R\)[ ]*)"
        ),
        r"([ ]*EATTR[ ]*)",
    ]
    data_set_info.update(_parse_table_row(regex_for_rows[0], lines[0], lines[1]))
    data_set_info.update(_parse_table_row(regex_for_rows[1], lines[2], lines[3]))
    data_set_info.update(_parse_table_row(regex_for_rows[2], lines[4], lines[5]))
    data_set_info.update(_parse_extents(lines[6:]))
    return data_set_info


def _parse_table_row(regex, header_row, data_row):
    """Parse out a single row of VTOC table information from
    VTOCLIST output.

    Arguments:
        regex {str} -- The regular expression used to parse table row.
        header_row {str} -- The row of the table containing headers.
        data_row {str} -- The row of the table containing data.

    Returns:
        dict -- Structured data for the row of the table.
    """
    table_data = {}
    fields = re.findall(regex, header_row)

    if len(fields) > 0:
        if isinstance(fields[0], str):
            fields = [[fields[0]]]
        count = 0
        for field in fields[0]:
            end = count + len(field)
            table_data[field.strip(" -0")] = data_row[count:end].strip()
            count += len(field)
    table_data = _format_table_data(table_data)
    return table_data


def _format_table_data(table_data):
    """Perform additional formatting on table data.
    This includes separating and renaming fields from
    their original naming and style in VTOCLIST.

    Arguments:
        table_data {dict} -- Structured data parsed from VTOCLIST output.

    Returns:
        dict -- Updated data.
    """
    handlers = {
        "DATA SET NAME": "data_set_name",
        "SER NO": "volume",
        "SEQNO": "sequence",
        "DATE.CRE": "creation_date",
        "DATE.EXP": "expiration_date",
        "DATE.REF": "last_referenced_date",
        "EXT": "number_of_extents",
        "DSORG": "data_set_organization",
        "RECFM": "record_format",
        "OPTCD": "option_code",
        "BLKSIZE": "block_size",
        "SMS.IND": "sms_attributes",
        "LRECL": "record_length",
        "KEYLEN": "key_length",
        "INITIAL ALLOC": "space_type",
        "2ND ALLOC": "space_secondary",
        "EXTEND": _format_extend,
        "LAST BLK(T-R-L)": {"name": "last_block_pointer", "func": _format_last_blk},
        "DIR.REM": "last_directory_block_bytes_used",
        "F2 OR F3(C-H-R)": {"name": "dscb_format_2_or_3", "func": _format_f2_or_f3},
        "DSCB(C-H-R)": {"name": "dscb_format_1_or_8", "func": _format_dscb},
        "EATTR": "extended_attributes",
    }
    formatted_table_data = {}
    for key, value in table_data.items():
        if not value:
            continue
        updated_data_item = handlers.get(key, key)
        if isinstance(updated_data_item, str):  # only need to update name
            formatted_table_data[updated_data_item] = value
        elif isinstance(updated_data_item, dict):  # need to update value, name defined
            updated_value = updated_data_item.get("func")(value)
            if not updated_value:
                continue
            formatted_table_data[updated_data_item.get("name")] = updated_value
        else:  # need to determine name and value
            formatted_table_data = updated_data_item(value, formatted_table_data)
    return formatted_table_data


def _format_extend(contents, formatted_table_data):
    """Format the extend field from VTOCLIST.

    Arguments:
        contents {str} -- Contents of the extend field from VTOCLIST.
        formatted_table_data {dict} -- The dictionary containing other already formatted
        table data.

    Returns:
        dict -- The updated formatted_table_data dictionary.
    """
    matches = re.search(r"([0-9]+)(AV|BY|KB|MB)", contents)
    original_space_secondary = ""
    average_block_size = ""
    if matches:
        if matches.group(2) == "AV":
            average_block_size = matches.group(1)
        elif matches.group(2) == "BY":
            original_space_secondary = matches.group(1) + "B"
        elif matches.group(2) == "KB":
            original_space_secondary = matches.group(1) + "KB"
        elif matches.group(2) == "MB":
            original_space_secondary = matches.group(1) + "MB"
    if original_space_secondary:
        formatted_table_data["original_space_secondary"] = original_space_secondary
    if average_block_size:
        formatted_table_data["average_block_size"] = average_block_size
    return formatted_table_data


def _format_last_blk(contents):
    """Format the last blk field from VTOCLIST.

    Arguments:
        contents {str} -- Contents of the last blk field from VTOCLIST.

    Returns:
        dict -- Structured data parsed from last blk field contents.
    """
    result = None
    matches = re.search(r"[ ]*([0-9]+)[ ]+([0-9]+)[ ]+([0-9]+)?", contents)
    if matches:
        result = {}
        result["track"] = matches.group(1)
        result["block"] = matches.group(2)
        if matches.group(3):
            result["bytes_remaining"] = matches.group(3)
    return result


def _format_f2_or_f3(contents):
    """Format the F2 or F3 field from VTOCLIST.

    Arguments:
        contents {str} -- Contents of the F2 or F3 field from VTOCLIST.

    Returns:
        dict -- Structured data parsed from the F2 or F3 field contents.
    """
    result = None
    matches = re.search(r"[ ]*([0-9]+)[ ]+([0-9]+)[ ]+([0-9]+)", contents)
    if matches:
        result = {}
        result["cylinder"] = matches.group(1)
        result["track"] = matches.group(2)
        result["record"] = matches.group(3)
    return result


def _format_dscb(contents):
    """Format the dscb field from VTOCLIST.

    Arguments:
        contents {str} -- Contents of the dscb field from VTOCLIST.

    Returns:
        dict -- Structured data parsed from the dscb field contents.
    """
    result = None
    matches = re.search(r"[ ]*([0-9]+)[ ]+([0-9]+)[ ]+([0-9]+)", contents)
    if matches:
        result = {}
        result["cylinder"] = matches.group(1)
        result["track"] = matches.group(2)
        result["record"] = matches.group(3)
    return result


def _parse_extents(lines):
    """Parse and structure extent data from VTOCLIST.

    Arguments:
        contents {list[str]} -- Partial contents of single data set section
        from VTOCLIST that will contain extent information if data set has
        extents.

    Returns:
        list[dict] -- Structured data parsed from the extent field contents.
    """
    extents = []
    if re.search(r"THE\sABOVE\sDATASET\sHAS\sNO\sEXTENTS", "".join(lines)):
        return {}
    regex_for_extents_indent = (
        r"(0\s*EXTENTS\s+)(?:(NO\s+)(LOW\(C-H\)\s+)(HIGH\(C-H\)[ ]*))"
    )
    regex_for_header_row = r"(NO\s+)(LOW\(C-H\)\s+)(HIGH\(C-H\)[ ]*)"
    indent_group = re.findall(regex_for_extents_indent, lines[0])
    indent_length = len(indent_group[0][0])
    header_groups = re.findall(regex_for_header_row, lines[0])
    regex_for_extents_data = _extent_regex_builder(indent_length, header_groups)
    extent_data = re.findall(regex_for_extents_data, "\n".join(lines), re.MULTILINE)
    if len(extent_data) > 0:
        extents = _format_extent_data(extent_data)
    return {"extents": extents}


def _extent_regex_builder(indent_length, header_groups):
    """Build regular expressions for parsing extent information.

    Arguments:
        indent_length {int} -- The number of spaces before extent information starts.
        header_groups {list[tuple]} -- Captured output of header groups identified
        during VTOCLIST parsing.

    Returns:
        str -- The regular expression for parsing extent information.
    """
    extent_regex = "^[ ]{{{0}}}".format(str(indent_length))
    for index, header_group in enumerate(header_groups):
        group_regex = "([ 0-9]{{{0}}})([ 0-9]{{{1}}})([ 0-9]{{{2}}})".format(
            *[str(len(x)) for x in header_group]
        )
        if index > 0:
            group_regex = "(?:{0}){{0,1}}".format(group_regex)
        extent_regex += group_regex
    extent_regex += "$"
    return extent_regex


def _format_extent_data(extent_data):
    """Format the dscb field from VTOCLIST.

    Arguments:
        extent_data {list[tuple]} -- Captured output of extent data.

    Returns:
        dict -- Structured data parsed from captured output of extent data.
    """
    extents = []
    flattened_extent_data = []
    for extent in extent_data:
        reduced_extent = [x.strip() for x in extent if x.strip() != ""]
        flattened_extent_data = flattened_extent_data + reduced_extent
    for index in range(int(len(flattened_extent_data) / 3)):
        position = index * 3
        extent = {}
        extent["number"] = flattened_extent_data[position]
        low = re.search(
            r"[ ]*([0-9]+)[ ]+([0-9]+)", flattened_extent_data[position + 1]
        )
        extent["low"] = {"cylinder": low.group(1), "track": low.group(2)}
        high = re.search(
            r"[ ]*([0-9]+)[ ]+([0-9]+)", flattened_extent_data[position + 2]
        )
        extent["high"] = {"cylinder": high.group(1), "track": high.group(2)}
        extents.append(extent)
    return extents


def _get_output(contents):
    output = ""
    # check if they passed full output, list or str
    if isinstance(contents, dict):
        output = contents.get("stdout", "")
    elif isinstance(contents, list):
        output = "\n".join(contents)
    elif isinstance(contents, str):
        output = contents
    return output


def get_datasets(vtoc_output):
    """Get dataset info from VTOC

    Args:
        listuser_output: The response object of shell or command module which ran IEHLIST.

    Returns:
        list[dict]: A list of data sets and their attributes.
    """
    output = _get_output(vtoc_output)
    data_sets = []
    try:
        data_sets = _process_output(output)
    except Exception:
        pass
    return data_sets

def past_date(date, days=None):
    if days:
        return date_will_expire_in_next_x_days(date, days)
    return is_date_expired(date)

def is_date_expired(date):
    if re.match(r'[0\.]+', date):
        return True
    present = datetime.datetime.now()
    date = datetime.datetime.strptime(date, '%Y.%j')
    return date < present

def date_will_expire_in_next_x_days(date, days):
    if re.match(r'[0\.]+', date):
        return True
    baseline = datetime.datetime.now() + datetime.timedelta(days=days)
    date = datetime.datetime.strptime(date, '%Y.%j')
    return date < baseline


def get_datasets_not_used_in_x_days(vtoc_output, days=60):
    output = _get_output(vtoc_output)
    data_sets = []
    not_used_data_sets = []
    try:
        data_sets = _process_output(output)
        for data_set in data_sets:
            referenced_date = data_set.get("last_referenced_date", "0")
            if date_will_expire_in_next_x_days(referenced_date, days):
                not_used_data_sets.append(data_set)
    except Exception:
        pass
    return not_used_data_sets

def get_users(listuser_output):
    """Parse user information from TSO LISTUSER command.

    Args:
        listuser_output: The response object of shell or command module which ran TSO LISTUSER.

    Returns:
        list[dict]: A list of users with their ID and Name attributes.
    """
    users = []
    output = _get_output(listuser_output)
    user_matches = re.findall(
        r"^USER\=([A-Za-z0-9\@\#\$]+)\s+NAME\=(.+?(?=OWNER\=)).*$", output, re.MULTILINE
    )
    for match in user_matches:
        user = {}
        user["id"] = match[0]
        user["name"] = match[1].strip()
        users.append(user)
    return users


def get_volumes(display_output):
    """Parse volume information from "d u,dasd" command.

    Args:
        display_output: The response object of shell or command module which ran "d u,dasd".

    Returns:
        list[dict]: A list of volumes and their attributes.
    """
    volumes = []
    output = _get_output(display_output)

    volume_matches = re.findall(
        r"^\s+([0-9ABCDEF]+)\s+([0-9]+)\s+[A-Z0-9]+\s+([A-Z0-9\@\#\$]+)\s+([[A-Z\/]+).*$",
        output,
        re.MULTILINE,
    )
    for match in volume_matches:
        volume = {}
        volume["unit"] = match[0]
        volume["type"] = match[1]
        volume["volser"] = match[2]
        volume["volstate"] = match[3]
        volumes.append(volume)
    return volumes


class FilterModule(object):
    """ Jinja2 filters for parsing system information """

    def filters(self):
        filters = {
            "get_users": get_users,
            "get_volumes": get_volumes,
            "get_datasets": get_datasets,
            "get_datasets_not_used_in_x_days": get_datasets_not_used_in_x_days,
        }
        return filters
