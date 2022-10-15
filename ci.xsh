#!/usr/bin/env xonsh
# -*- coding: utf-8 -*-
"""This script moves the results from a CI run into the right folder."""

from datetime import datetime
from jinja2 import Environment, FileSystemLoader
from mdtable import MDTable

baseDir = "./assets"
ingestDir = f"{baseDir}/ingest/combined-results"
current_date = datetime.now()
current_date_str = f"{current_date.strftime('%Y-%m-%dT%H%M%S%z')}"

if !(test -e @(ingestDir)):
  # move the folder to the right place
  mv @(ingestDir) @(baseDir)/@(current_date_str)

  # remove json data
  rm @(baseDir)/@(current_date_str)/*.json

  # Creat symlink to the latest folder
  cd @(baseDir)
  rm -rf latest
  ln -sf @(current_date_str) latest
  cd ..

file_loader = FileSystemLoader("templates")
env = Environment(loader=file_loader)

#------- START: Generate single entry
template = env.get_template("single_page.tpl.md")
data = {}
data['date'] = f"{current_date.strftime('%Y-%m-%d %H:%M:%S  %Z')}"
data['folder_date'] = current_date_str
data['table'] = MDTable(f"./assets/{current_date_str}/combined_results.csv").get_table()

# render data to file
output = template.render(data=data)
with open(f"./pages/{current_date_str}.md", "w") as f:
  f.write(output)
#------- END

#------- START: Generate index
template = env.get_template("index.tpl.md")
data = {}
data['date'] = f"{current_date.strftime('%Y-%m-%d %H:%M:%S  %Z')}"
data['table'] = MDTable(f"./assets/latest/combined_results.csv").get_table()

# render data to file
output = template.render(data=data)
with open("./index.md", "w") as f:
  f.write(output)
#------- END

# Output link to Github page
print(f"https://niklas-heer.github.io/speed-comparison/")
