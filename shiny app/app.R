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

# Define UI
ui <- fluidPage(
  titlePanel("Mental Health Panels"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", label = "Age", min = 0, max = 100, value = c(0, 100)),
      sliderInput("hours_per_day", label = "Hours per day", min = 0, max = 12, value = c(0, 12))
    ),
    mainPanel(
      plotOutput("scatter1"),     # First static plot
      plotlyOutput("interactive") # Interactive plot using ggplotly
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
    ggplot(data = filtered_data(),
           mapping = aes(x = age, y = hours_per_day, color = age, size = hours_per_day)) +
      geom_point(alpha = 0.3) +
      labs(title = "Age vs Hours per Day", x = "Age", y = "Hours per Day") +
      theme_minimal()
  })
  
  # Render the interactive plot using ggplotly
  output$interactive <- renderPlotly({
    music_effect_plot <- ggplot(data = filtered_data(),
                                mapping = aes(
                                  x = age, 
                                  y = hours_per_day, 
                                  color = music_effects)) +
      geom_point(alpha = 0.7) +
      labs(title = "Interactive Music Effects Plot", 
           x = "Age", 
           y = "Hours per Day") +
      theme_minimal()
    
    ggplotly(music_effect_plot) # Correctly pass the full ggplot object to ggplotly()
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
