library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

sensitivity_ui <- shiny::tabPanel(
    title = "Sensitivity",
    shiny::fluidRow(
        shinydashboard::box(id = "sensitivity_options_box", width = 12,
                            shiny::fluidRow(
                                style='margin-top: 18px;',
                                align = 'center',
                                shiny::column(
                                    width = 4,
                                    shinyWidgets::numericRangeInput("snp_range", "Max SNP distance range", 
                                                                    value = c(0, 28), min = 0, max = 100)
                                ),
                                shiny::column(
                                    width = 4,
                                    shinyWidgets::numericRangeInput("date_range", "Max temporal distance range (days)", 
                                                                    value = c(0, 28), min = 0, max = 100)
                                ),
                                shiny::column(
                                    width = 4,
                                    shiny::numericInput("interval", "Interval between ranges",
                                                        value = 1, min = 1, max = 10)
                                ),
                            ),
        ),
    ),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("cluster_prop_sensitivity_heatmap") %>% shinycssloaders::withSpinner()
        ),
        shiny::column(
            width = 12,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("transmission_prop_sensitivity_heatmap") %>% shinycssloaders::withSpinner()
        ),
    )
)