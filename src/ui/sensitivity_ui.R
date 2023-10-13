library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

sensitivity_ui <- shiny::tabPanel(
    title = "Sensitivity",
    shiny::fluidRow(
        shinydashboard::box(
            id = "sensitivity_options_box", width = 12,
            shiny::fluidRow(
                style='margin-top: 18px;',
                align = 'center',
                shiny::column(
                    width = 4,
                    shiny::uiOutput("date_threshold")
                ),
                shiny::column(
                    width = 4,
                    shiny::uiOutput("date_threshold_range"),
                ),
                shiny::column(
                    width = 4,
                    shiny::uiOutput("ext_date_threshold_range"),
                ),
            ),
        ),
    ),
    # ribbon graphs
    shiny::fluidRow(
        shiny::column(
            width = 6,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("cluster_prop_SNP_plot", height = "300px") %>%
                shinycssloaders::withSpinner()
        ),
        shiny::column(
            width = 6,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("transmission_prop_SNP_plot", height = "300px") %>%
                shinycssloaders::withSpinner()
        ),
    ),
    # download sensitivity data
    shiny::fluidRow(
        shiny::column(
            width = 4,
            shiny::uiOutput("download_sensitivity_data_button")
        )
    )
)