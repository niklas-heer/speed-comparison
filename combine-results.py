#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""This script combines the JSON results in a given directory into one csv ."""

import os
import json
import pandas as pd
from datetime import datetime
from argparse import ArgumentParser


def main():
    parser = ArgumentParser()
    parser.add_argument(
        "--folder",
        dest="folder",
        help="Path to folder which contains JSON files.",
    )
    parser.add_argument(
        "--out",
        dest="out",
        help="Path to generate output file to.",
    )

    args = parser.parse_args()

    data = {
        "name": [],
        "version": [],
        "average": [],
        "accuracy": [],
    }

    # r=root, d=directories, f = files
    for r, d, f in os.walk(args.folder):
        for file in f:
            if file.endswith(".json"):
                with open(os.path.join(r, file), "r") as reader:
                    # TODO: Add check if the file is formatted correctly
                    json_data = json.load(reader)
                    data["name"].append(json_data["Language"])
                    data["version"].append(json_data["Version"])
                    data["average"].append(pd.Timedelta(json_data["Average"]))
                    data["accuracy"].append(json_data["Accuracy"])

    df = pd.DataFrame(data)
    output_file = f"{args.out}/{datetime.now().strftime('%Y-%m-%d_%H:%M')}.csv"
    df.to_csv(
        output_file,
        index=False,
        encoding="utf-8",
    )

    print(f"Done. Results exported to: {output_file}")


main()
