#
#
#    LINELIST DGPH R4EPI EXAMPLE
#
#
# Version 0.1
# 2024-11-29


# Load Packages -------------------------
pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  matchmaker, # dictionary-based cleaning
  epikit, # age_categories() function
  tidyverse, # data management and visualization
  #
  styler, # source code formatting
  lintr, # detects bad code patterns, which are not errors
  #
  skimr, # previdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCCew tibbles (aka data frames)
  todor # add TODO comments to your project
)


# Import Data ---------------------------
# TODO: check wrong file on import
linelist_raw <- import("linelist_raw.xlsx")
skimr::skim(linelist_raw)


# Clean Data ----------------------------
