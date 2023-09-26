library(shiny)
library(shinyWidgets)

sidebar_ui <- shiny::sidebarPanel(
    width = 3,
    shiny::fluidRow(
        shiny::column(12, align = "left",
            h4('Select dataset'),
            shinyWidgets::radioGroupButtons(
                inputId = "data_option",
                label = NULL,
                choices = c("Use demo dataset", "Upload dataset"),
                selected = "Use demo dataset",
                individual = TRUE
            ),
            shiny::conditionalPanel(
                condition = "input.data_option == 'Upload dataset' && output.data_loaded != true",
                shiny::fileInput(
                    inputId = "user_distances", 
                    label = "Upload distances (tsv/csv)"
                ),
                shiny::fileInput(
                    inputId = "user_metadata", 
                    label = "Upload metadata (tsv/csv)"
                ),
                shiny::fileInput(
                    inputId = "user_kleborate_data", 
                    label = "Upload Kleborate data (tsv/csv)"
                ),
            )
        )
    ), # End fluidRow - Dataset selection
    shiny::conditionalPanel(
        condition = "output.data_loaded == true",
        shiny::fluidRow(
            shiny::column(
                width=12,
                h4("Filter data"),
                shiny::uiOutput("filter_data_columns"),
                shiny::uiOutput("filter_data_options"),
                shiny::actionButton(inputId = "apply_filters", 
                                    label = "Apply filters", 
                                    class = 'filter-button')
            ),
        ), # End fluidRow - Filter data
    ) # End conditionalPanel 

    
)