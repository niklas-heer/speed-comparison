#!/usr/bin/env xonsh
# -*- coding: utf-8 -*-
"""This script moves the results from a CI run into the right folder."""

from datetime import datetime
from jinja2 import Environment, FileSystemLoader
from mdtable import MDTable

baseDir = "./assets"
ingestDir = f"{baseDir}/ingest/combined-results"
current_date_str = f"{datetime.now().strftime('%Y-%m-%dT%H%M%S%z')}"

if !(test -e @(ingestDir)):
  # move the folder to the right place
  mv @(ingestDir) @(baseDir)/@(current_date_str)

  # Creat symlink to the latest folder
  cd @(baseDir)
  rm -rf latest
  ln -sf @(current_date_str) latest
  cd ..

# Render template
file_loader = FileSystemLoader("templates")
env = Environment(loader=file_loader)
template = env.get_template("index.tpl.md")

data = {}
data['date'] = current_date_str

# generate markdown table from csv
data['latest_table'] = MDTable(f"./assets/latest/combined_results.csv").get_table()

# render data to file
output = template.render(data=data)
with open("./index.md", "w") as f:
  f.write(output)

# Output link to Github page
print(f"https://niklas-heer.github.io/speed-comparison/")
