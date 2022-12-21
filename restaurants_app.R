#'*Problem 4 - Restaurant Violations app*
library(pacman)
p_load(tidyverse, shiny, shinybusy, mdsr)

mergedViolations <- Violations %>%
  left_join(Cuisines)

modified_Violations <- mergedViolations |> 
  group_by(dba, boro, cuisine_description) |>
  summarize(count = n())


ui <- fluidPage(
  titlePanel("Restaurant Explorer"),
  fluidRow(
    # some things take time: this lets users know
    add_busy_spinner(spin = "fading-circle"),
    column(width = 4, selectInput(inputId = "boro",
                                  label = "Borough:",
                                  choices = c("ALL",
                                              unique(as.character(modified_Violations$boro))
                                  )
    )
    ),
    # display dynamic list of cuisines
    column(width = 4, uiOutput("cuisinecontrols"))
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)



server <- function(input, output) {
  datasetboro <- reactive({  # Filter data based on selections
    
    data <- modified_Violations
    req(input$boro)  # wait until there's a selection
    
    if (input$boro != "ALL") {
      data <- data |>
        group_by(dba, boro) |>
        count(cuisine_description) |>
        filter(boro == input$boro) 
    }
    data
  })
  
  datasetcuisine <- reactive({  # dynamic list of cuisines
    req(input$cuisine)   # wait until list is available
    data <- datasetboro() %>%
      unique()
    
    if (input$cuisine != "ALL") {
      data <- data |>
        group_by(dba, cuisine_description) |> 
        count(cuisine_description) |>
        filter(cuisine_description == input$cuisine)
    }
    data
  })
  
  output$table <- DT::renderDataTable(DT::datatable(datasetcuisine()))
  
  output$cuisinecontrols <- renderUI({
    availablelevels <-
      unique(sort(as.character(datasetboro()$cuisine_description)))
    selectInput(
      inputId = "cuisine",
      label = "Cuisine:",
      choices = c("ALL", availablelevels)
    )
  })
}

shinyApp(ui, server)