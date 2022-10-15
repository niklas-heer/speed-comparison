#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""This script analyzes the JSON results in a given directory."""

import os
import re
import json
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime
from argparse import ArgumentParser


def colors_from_values(values, palette_name):
    # normalize the values to range [0, 1]
    normalized = (values - min(values)) / (max(values) - min(values))
    # convert to indices
    indices = np.round(normalized * (len(values) - 1)).astype(np.int32)
    # use the indices to get the colors
    palette = sns.color_palette(palette_name, len(values))
    return np.array(palette).take(indices, axis=0)


def plot(df, rounds, to_file):
    # Theme
    sns.set(style="dark", context="notebook")
    sns.set_color_codes("pastel")
    plt.style.use("dark_background")

    # Plot
    # TODO: Find a way to display the platte scale to highly accuracy better.
    bar = sns.barplot(
        x="average",
        y="name",
        data=df,
        width=1,
        edgecolor="black",
        linewidth=2,
        errwidth=0,
        log=True,
        # TODO: Improve color palette.
        # https://seaborn.pydata.org/tutorial/color_palettes.html
        palette=colors_from_values(df["accuracy"], "light:seagreen"),
    )
    bar.bar_label(
        bar.containers[0],
        fontsize=10,
        padding=3,
    )
    plt.xlabel("Average time (ms) in log scale", fontweight="bold")
    plt.ylabel("Language", fontweight="bold")

    # Title
    plt.suptitle(
        "Speed comparison of various programming languages\n",
        fontweight="bold",
        fontsize=20,
        y=1.02,
    )
    plt.title(
        f"Method: calculating π through the Leibniz formula {rounds} times",
        style="italic",
        fontsize=16,
        y=1.02,
    )

    # Caption
    url = f"https://github.com/niklas-heer/speed-comparison"
    plt.figtext(
        0.5, -0.1, url, wrap=True, horizontalalignment="left", fontsize=12
    )
    timestamp = f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}"
    plt.figtext(
        0.2,
        -0.1,
        timestamp,
        wrap=True,
        horizontalalignment="right",
        fontsize=12,
    )

    sns.despine()
    plt.autoscale()
    plt.savefig(to_file, pad_inches=0.2, bbox_inches="tight", dpi=200)


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
    parser.add_argument(
        "--rounds",
        dest="rounds",
        help="Path to the rounds.txt file.",
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
                    data["average"].append(
                        # We want milliseconds (ms) in the end
                        pd.Timedelta(json_data["Average"]).total_seconds()
                        * 1000
                    )
                    data["accuracy"].append(json_data["Accuracy"])

    df = pd.DataFrame(data)
    df.sort_values(by=["average"], inplace=True)

    # yyyy-MM-ddTHHmmssZ
    file_base = f"{args.out}/{datetime.now().strftime('%Y-%m-%dT%H%M%S%z')}"
    png = f"{file_base}.png"
    csv = f"{file_base}.csv"
    df.to_csv(
        csv,
        index=False,
        encoding="utf-8",
    )

    # Visualize
    rounds = 0
    with open(args.rounds, "r") as reader:
        rounds = reader.read().strip()
    plot(df, rounds, png)

    print(f"Successfult. Files generated:\n  {csv}\n  {png}")


main()
