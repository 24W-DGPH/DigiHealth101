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
  tidyverse, # data management and visualizations
  styler, # source code formatting
  lintr, # detects bad code pattern
  skimr, # preview tibbles
  ggplot2, # data visualization
  zoo, # extra date functions
  plotly,   #interactive plots
  shiny   # shiny app
)
# Import data ------------------
  mental_health <- import("mental_health.csv")
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


mental_health$date <- as.Date(mental_health$timestamp)    # create date and time as two column
mental_health$time <- format(as.POSIXct(mental_health$timestamp),
               format = "%H:%M:%S")

mental_health = select(mental_health, -1)      #delete column

mental_health = select(mental_health, permissions, everything())   #relocate permission

mental_health = relocate(mental_health, date)   #relocate date

mental_health = relocate(mental_health, time, .after = date)   #relocate time

mental_health %>% 
  select(where(is.numeric)) %>%           # check columns which are class numeric
  names()

mental_health %>% 
  select(contains("date")) %>% 
  names()

mental_health = select(mental_health, -3)


class(mental_health$hours_per_day)

class(mental_health$age)

mental_health <- mental_health %>% 
  na.omit(mental_health)             # omit all variable with missing values



# Data visualization ---------------


is.na(mental_health)   # find missing values

sum(is.na(mental_health))    #total number of missing values

na.omit(mental_health)   #omit missing values

mean(mental_health$age, na.rm = TRUE)    # calculate mean of age

mean(mental_health$hours_per_day, na.rm = TRUE)   #calculate mean of hours per day

mean(mental_health$bpm, na.rm = TRUE)    #mean of beats per minute

ggplot(data = mental_health, mapping = aes(x =age, y = hours_per_day))+
  geom_point(color =  "purple") 

ggplot(data = mental_health, mapping = aes(x = age))+
  geom_histogram(
    binwidth = 6,
    color = "red",
    fill = "lightblue"
  ) 

ggplot(data = mental_health, mapping = aes(x = primary_streaming_service)) +
  geom_bar(mapping = aes(fill = foreign_languages), color = "blue", na.rm = TRUE)

ggplot(data = mental_health, 
       mapping = aes(     #map aesthetics to column values
         x =age,         #map x axis to age
         y = hours_per_day,         #map y axis to hours_per_day
         color = primary_streaming_service,    #map color to primary_streaming_service
         size = primary_streaming_service))+   #map size to primary_streaming_service
  geom_point(                      #display data as points
    alpha = 0.3)

age_count_plot <- ggplot(data = mental_health, mapping = aes(x = age))+
  geom_histogram(
    binwidth = 6,
    color = "red",
    fill = "lightblue"
  )

age_count_plot %>% plotly::ggplotly()


music_effect_plot <- 
  ggplot(data = mental_health, 
         mapping = aes(     #map aesthetics to column values
           x =age,         #map x axis to age
           y = hours_per_day,         #map y axis to hours_per_day
           color = music_effects,  #map color to music effects
           size = 0.3))+   
  geom_point(                      #display data as points
    alpha = 0.7)
music_effect_plot %>% plotly::ggplotly()

ggplot(data = mental_health, aes(x = `hours_per_day`, y = anxiety, color = `music_effects`)) +
  geom_point(alpha = 0.7) +
  labs(title = "Relationship Between Music Hours and Anxiety",
       x = "hours_per_day",
       y = "anxiety") +
  theme_minimal()

ggplot(data = mental_health, aes(x = `favourite_genre`, fill = `music_effects`)) +
  geom_bar() +
  labs(title = "Favorite Music Genres and Their Effects on Mental Health",
       x = "Favorite Genre",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mental_health, aes(x = age, y = hours_per_day, color = primary_streaming_service)) +
  geom_point(size = 3) +
  labs(title = "Streaming Hours vs Age",
       x = "Age",
       y = "Hours per Day",
       color = "Streaming Service") +
  theme_minimal()

