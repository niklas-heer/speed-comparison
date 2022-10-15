#!/usr/bin/env xonsh
# -*- coding: utf-8 -*-
"""This script moves the results from a CI run into the right folder."""

from datetime import datetime


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

# Output link to Github page
print(f"https://niklas-heer.github.io/speed-comparison/")
