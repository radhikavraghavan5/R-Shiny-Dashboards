#'*Problem 7 - Penguins Measurements*
library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Penguins Measurements"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "species", label = "Select a Species", choices = unique(palmerpenguins::penguins$species)),
      selectInput(inputId = "xcol", label = "select X variable", choices = colnames(palmerpenguins::penguins)),
      selectInput(inputId = "ycol", label = "select Y variable", choices = colnames(palmerpenguins::penguins))
      # sliderInput("year", 
      #             label = "Year Range:", 
      #             min = min(penguins$year), 
      #             max = max(penguins$year), 
      #             value = c(min(penguins$year),
      #                       max(penguins$year)),
      #             sep = "",
      #             step = 1)
    ),
    
    mainPanel(
      plotOutput(outputId = "plot")
    )
    
  )
)


server <- function(input, output) {
  
  
  # Fill in the spot we created for a plot
  output$plot <- renderPlot({
    
    palmerpenguins::penguins |>
      filter(species == input$species) |>
      
      #Render the scatterplot
      ggplot(aes(x = .data[[input$xcol]],
                 y = .data[[input$ycol]],
                 color = species)) +
      geom_point() +
      theme_minimal()
  })
}

#runs the application
shinyApp(ui, server)