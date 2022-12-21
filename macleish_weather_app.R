#'*Problem 8 - Macleish Weather App*
library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Macleish Weather App"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset", 
                  label = "Select a dataset", 
                  choices = c("whately_2015" = "whately_2015", "orchard_2015" = "orchard_2015")
      ),
      selectInput(inputId = "xcol", 
                  label = "select X variable", 
                  choices = colnames(macleish::orchard_2015)
      ),
      selectInput(inputId = "ycol", 
                  label = "select Y variable", 
                  choices = colnames(macleish::whately_2015)
      ),
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = "2015-01-01", 
                     end = "2015-12-31",
                     min = "2015-01-01",
                     max = "2015-12-31"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "plot")
    )
  )
)

server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "whately_2015" = macleish::whately_2015, 
           "orchard_2015" = macleish::orchard_2015) # |>
    
    # filter(when>input$dateRange[1], when<input$dateRange[2])
    
  })
  
  # Fill in the spot we created for a plot
  output$plot <- renderPlot({
    filteredDataset <- datasetInput() |> filter(when>input$dateRange[1], when<input$dateRange[2])
    #Render the scatterplot
    ggplot(filteredDataset, 
           aes(x = .data[[input$xcol]],
               y = .data[[input$ycol]]
               )
           ) +
      geom_point() +
      theme_minimal()
  })
}

#runs the application
shinyApp(ui, server)