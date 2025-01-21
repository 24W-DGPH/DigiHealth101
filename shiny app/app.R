# Load necessary packages
pacman::p_load(
  rio,  # data import
  here, # relative file pathway
  janitor, # cleans data
  lubridate, # working with dates
  epikit, # age_categories() function
  matchmaker, # dictionary-based cleaning
  dplyr, # data management
  ggplot2, # data visualization
  shiny   # shiny app
)

# Assuming 'mental_health' dataset is loaded previously in the code
source("mental_health.R")

library(shiny)

# UI definition
ui <- fluidPage(
  titlePanel("Mental Health Panel"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", label = "Age", min = 0, max = 100, value = c(0, 100)),
      sliderInput("hours_per_day", label = "Hours per day", min = 0, max = 12, value = c(0, 12))
    ),
    mainPanel(
      plotOutput("scatter1")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Function to create scatter plot
  scatter <- function(mental_health, age_filter, hours_filter) {
    filtered_data <- mental_health %>% 
      filter(age >= age_filter[1] & age <= age_filter[2],
             hours_per_day >= hours_filter[1] & hours_per_day <= hours_filter[2])
    
    ggplot(data = filtered_data, 
           mapping = aes(x = age, y = hours_per_day, color = age, size = hours_per_day)) +
      geom_point(alpha = 0.3)
  }
  
  # Reactive function to filter data and generate the plot
  scatter_filter <- reactive({
    scatter(mental_health, input$age, input$hours_per_day)
  })
  
  # Render the scatter plot
  output$scatter1 <- renderPlot({
    scatter_filter()
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)
