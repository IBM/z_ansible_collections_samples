#!/usr/bin/env python3
# Copyright (c) IBM Corporation 2020
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

import json
import csv
from os import listdir
import argparse


def read_datasets_from_json(input_file):
    datasets = []
    with open(input_file, "r") as json_file:
        datasets = json.load(json_file)
    return datasets


def flatten_contents(contents):
    for item in contents:
        if not isinstance(item, (str, int)):
            item = json.dumps(item)
    return contents


def build_contents(headers, dataset):
    contents = []
    for header in headers:
        contents.append(dataset.get(header, ""))
    return contents


def write_headers_to_csv(headers, output_file):
    with open(output_file, "w+") as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(headers)


def append_datasets_to_csv(headers, datasets, output_file):
    with open(output_file, "a+") as csv_file:
        csv_writer = csv.writer(csv_file)
        for dataset in datasets:
            content = flatten_contents(build_contents(headers, dataset))
            try:
                csv_writer.writerow(content)
            except UnicodeEncodeError:
                pass


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Combine JSON files into single CSV")
    parser.add_argument(
        "directory", type=str, help="The directory to search for JSON files."
    )
    parser.add_argument(
        "csvname", type=str, help="The name to use for the CSV output file."
    )
    args = parser.parse_args()
    headers = []
    for input_file in listdir(args.directory):
        if input_file.endswith(".json"):
            datasets = read_datasets_from_json(args.directory + "/" + input_file)
            if not headers and datasets:
                headers = datasets[0].keys()
                write_headers_to_csv(headers, args.csvname)
            append_datasets_to_csv(headers, datasets, args.csvname)
