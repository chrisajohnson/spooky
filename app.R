# This is the R file for the spooky GitHub repo
# Attach necessary packages
library(tidyverse)
library(shiny)
library(shinythemes)

# Import data
spooky <- read_csv("spooky_data.csv")

# Create the user interface:
ui <- fluidPage(
  theme = shinytheme("slate"), # Background theme of app
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("put my widgets here",
                 # Select state drop-down menu
                 selectInput(inputId = "state_select",
                             label = "Choose a state",
                             choices = unique(spooky$state)
                 ),
                 # Radio button
                 radioButtons(inputId = "region_select",
                              label = "Choose region:",
                              choices = unique(spooky$region_us_census))
    ),
    mainPanel("put my outputs here",
              # Table of state's top candies
              p("State's top candies:"),
              tableOutput(outputId = "candy_table"),
              # Graph of costume preference
              p("Region's top costumes:"),
              plotOutput(outputId = "costume_graph")
    )
  )
)

# Create the server function:
server <- function(input, output) {
  
  # Reactive object: getting input from user
  state_candy <- reactive({
    spooky %>%
      filter(state == input$state_select) %>% # subsetting data
      select(candy, pounds_candy_sold)
  })
  
  # Make table
  output$candy_table <- renderTable({
    state_candy()
  })
  
  # Make graph of costume preference
  region_costume <- reactive({
    spooky %>%
      filter(region_us_census == input$region_select) %>%
      count(costume, rank)
  })
  
  
  output$costume_graph <- renderPlot({
    ggplot(region_costume(), aes(x = costume, y = n)) +
      geom_col(aes(fill = rank)) +
      coord_flip() +
      scale_fill_manual(values = c("black","purple","orange")) +
      theme_minimal()
  })
}

# Combine them into an app:
shinyApp(ui = ui, server = server)

