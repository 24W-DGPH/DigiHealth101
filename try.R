# New Project
#
#01-05-2025
#
#
# Load Packages -----------------------
pacman::p_load(
  rio,  # data import
  here, # relative file pathway
  janitor, # cleans data
  lubridate, # working with dates
  epikit, # age_categories() function
  matchmaker, # dictionary based cleaning
  group_by, # groups one or more variables
  tidyverse, # data management and visualizations
  styler, # source code formatting
  lintr, # detects bad code pattern
  skimr, # preview tibbles
  ggplot2, # data visualization
  zoo, # extra date functions
  as.Date, #date manipulation
  as.POSIXct #date manipulation
)
# Import data ------------------
  mental_health <- import("mental health.csv")
  mental_health_dirty <- mental_health

skimr:: skim(mental_health_dirty)


# Clean data ---------------
mental_health <- mental_health_dirty %>% 
  
  janitor::clean_names() %>%   # clean dirty data
  rename(
   favourite_genre = fav_genre,
   frequency_rnb = frequency_r_b  #rename
  )
mental_health$timestamp <- strptime(as.character(mental_health$timestamp), "%m/%d/%Y %H:%M:%S")  #correct date format
format(mental_health$timestamp, "%Y/%m/%d %H:%M:%S")


mental_health$date <- as.Date(mental_health$timestamp)
mental_health$time <- format(as.POSIXct(mental_health$timestamp),
               format = "%H:%M:%S")

mental_health = select(mental_health, -1)

relocate(mental_health, date)