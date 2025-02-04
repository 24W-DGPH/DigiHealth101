# Load necessary packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  rio,  # data import
  here, # relative file pathway
  janitor, # cleans data
  lubridate, # working with dates
  epikit, # age_categories() function
  matchmaker, # dictionary-based cleaning
  tidyverse, # data management and visualizations
  styler, # source code formatting
  lintr, # detects bad code patterns
  skimr, # preview tibbles
  ggplot2, # data visualization
  zoo, # extra date functions
  plotly,   # interactive plots
  shiny,   # shiny app
  dplyr    # data manipulation
)

# Ensure the 'mental_health.csv' file is in the same directory
if (!file.exists("mental_health.csv")) {
  stop("The file 'mental_health.csv' was not found in the current working directory.")
}

# Load the dataset
mental_health <- read_csv("mental_health.csv") %>%
  clean_names() # Clean column names for consistency

# Check if required columns exist
required_columns <- c("age", "hours_per_day", "fav_genre", "music_effects", "primary_streaming_service")
missing_columns <- setdiff(required_columns, names(mental_health))

if (length(missing_columns) > 0) {
  stop(paste("The dataset is missing the following required columns:", 
             paste(missing_columns, collapse = ", ")))
}

# Define UI
ui <- fluidPage(
  titlePanel("Mental Health Panels"),
  tabsetPanel(
    tabPanel("Hours listening to music",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("age", label = "Age", min = 0, max = 100, value = c(0, 100)),
                 sliderInput("hours_per_day", label = "Hours per day", min = 0, max = 12, value = c(0, 12))
               ),
               mainPanel(plotOutput("scatter1"))
             )
    ),
    tabPanel("Effect of Music on Mental Health",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("age", label = "Age", min = 0, max = 100, value = c(0, 100))
               ),
               mainPanel(plotlyOutput("genre_effects"))
             )
    ),
    tabPanel("Primary Streaming Service",
             sidebarLayout(
               sidebarPanel(
                 selectInput("streaming_service", label = "Primary Streaming Service", 
                             choices = c("All", "Spotify", "Apple Music", "Pandora", "YouTube Music", "Other streaming service", "I do not use a streaming service."),
                             selected = "All")
               ),
               mainPanel(plotOutput("scatter2"))
             )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive filtering of the dataset
  filtered_data <- reactive({
    mental_health %>%
      filter(
        age >= input$age[1] & age <= input$age[2],
        hours_per_day >= input$hours_per_day[1] & hours_per_day <= input$hours_per_day[2]
      )
  })
  
  # Render the first scatter plot: Age vs Hours per Day
  output$scatter1 <- renderPlot({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for the selected filters.")
    )
    ggplot(data = filtered_data(),
           mapping = aes(x = age, y = hours_per_day, color = age, size = hours_per_day)) +
      geom_point(alpha = 0.3) +
      labs(title = "Age vs Hours per Day", x = "Age", y = "Hours per Day") +
      theme_minimal()
  })
  
  # Render the interactive bar plot for Favorite Genre and Music Effects
  output$genre_effects <- renderPlotly({
    validate(
      need(nrow(filtered_data()) > 0, "No data available for the selected filters."),
      need(!is.null(filtered_data()$fav_genre), "Missing 'fav_genre' column in data."),
      need(!is.null(filtered_data()$music_effects), "Missing 'music_effects' column in data.")
    )
    plot <- ggplot(data = filtered_data(), aes(x = fav_genre, fill = music_effects)) +
      geom_bar() +
      labs(title = "Favorite Music Genres and Their Effects on Mental Health",
           x = "Favorite Genre",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(plot)
  })
  
  # Render the third scatter plot: Age vs Hours per Day filtered by Streaming Service
  output$scatter2 <- renderPlot({
    filtered_streaming_data <- if (input$streaming_service == "All") {
      mental_health
    } else {
      mental_health %>%
        filter(primary_streaming_service == input$streaming_service)
    }
    
    validate(
      need(nrow(filtered_streaming_data) > 0, "No data available for the selected streaming service.")
    )
    
    ggplot(filtered_streaming_data, aes(x = age, y = hours_per_day, color = primary_streaming_service)) +
      geom_point(size = 3) +
      scale_color_manual(values = c(
        "Spotify" = "#1DB954",
        "Apple Music" = "#FF1493",
        "Pandora" = "#0057B7",
        "YouTube Music" = "#FF0000",
        "Other streaming service" = "#A020F0",
        "I do not use a streaming service." = "#FF8C00"
      )) +
      labs(title = "Streaming Hours vs Age (Filtered by Streaming Service)",
           x = "Age",
           y = "Hours per Day",
           color = "Streaming Service") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
