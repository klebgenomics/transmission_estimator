library(shiny)
library(shinyWidgets)

sidebar_ui <- shiny::sidebarPanel(
    width = 3,
    shiny::fluidRow( # Select data
        shiny::column(12, align = "left",
            h4('Select dataset'),
            shinyWidgets::radioGroupButtons(
                inputId = "data_option",
                label = NULL,
                choices = c("Demo dataset", "Upload dataset"),
                selected = "Demo dataset",
                individual = TRUE, checkIcon = list(
                    yes = icon("square-check")
                )
            ),
            shiny::conditionalPanel(
                condition = "input.data_option == 'Demo dataset'",
                tags$span(class="text-muted", 
                          "BARNARDS Study (",
                          tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/33782558/", 
                                 target = "_blank", "Sands et al. 2021"),
                          ")"
                ),
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
            ),
        )
    ),
    br(), br(),
    
    shiny::conditionalPanel( # Optional panel when data loaded
        condition = "output.data_loaded == true",
        shiny::fluidRow( # Filter data
            shiny::column(
                width=12,
                h4("Filter data"),
                shiny::uiOutput("filter_data_columns"),
                shiny::uiOutput("filter_data_options"),
                shiny::actionButton(inputId = "apply_filters", 
                                    label = "Apply filters", 
                                    class = 'my-button pry-button')
            ),
        ),
        hr(),
        shiny::conditionalPanel( # Hide summary when data empty
            condition = "output.data_empty != true",
            shiny::fluidRow( # Data summary
                shiny::column(
                    width=12,
                    h4("Data summary"),
                    shiny::tableOutput("data_summary")
                ),
            )
        ),
        shiny::conditionalPanel( # Show help when data empty
            condition = "output.data_empty == true",
            shiny::fluidRow(
                shiny::column(
                    width=12,
                    class = "text-danger",
                    p("No samples found with matching rows in the metadata, 
                      kleborate data, and SNP data. Please check the following:"),
                    tags$ul(
                        tags$li("Confirm that the uploaded files contain 
                                the expected data."),
                        tags$li("Ensure that sample names in each of the 
                                uploaded files are consistent."),
                        tags$li("Check for potential errors or missing information 
                                in the uploaded files."),
                        tags$li("If using filters, ensure they are correctly set 
                                and not excluding all samples.")
                    )
                ),
            )
        )
    ) 
)