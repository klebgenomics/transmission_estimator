library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

sensitivity_ui <- shiny::tabPanel(
    title = "Sensitivity",
    # Sensitivity options
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
    # Sub-tabs
    tags$style(HTML("
        #sensitivity_subtabs > li               > a {background-color: #EEEEEE; color:black}
        #sensitivity_subtabs > li[class=active] > a {background-color: black; color:white}
    ")),
    shiny::tabsetPanel(
        id = "sensitivity_subtabs", type = "pills",
        # Estimates sub-tab
        shiny::tabPanel(
            title = "Estimates", icon = icon("chart-line"),
            # ribbon graphs
            shiny::fluidRow(
                style='margin-top: 50px;',
                shiny::column(
                    width = 6,
                    style='margin-bottom: 50px;',
                    plotly::plotlyOutput("cluster_sensitivity_plot", height = "400px") %>%
                        shinycssloaders::withSpinner()
                ),
                shiny::column(
                    width = 6,
                    style='margin-bottom: 50px;',
                    plotly::plotlyOutput("transmission_sensitivity_plot", height = "400px") %>%
                        shinycssloaders::withSpinner()
                ),
            ),
            # download sensitivity data
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    div(shiny::uiOutput("download_sensitivity_data_and_plots")),
                ),
            ),
        ),
        # Compare estimates sub-tab
        shiny::tabPanel(
            title = "Compare estimates", icon = icon("balance-scale"),
            # comparison options
            shiny::fluidRow(
                style='margin-top: 18px;',
                shiny::column(
                    width=3,
                    shiny::uiOutput("comparison_var")
                ),
                shiny::column(
                    width=6,
                    div(tags$label(), style = "margin-bottom: 9px"),
                    shinyWidgets::materialSwitch(
                        inputId = "add_public_data_toggle",
                        label = "Include public data in comparison plot",
                        status = 'info')
                ),
            ),
            # plots
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    plotly::plotlyOutput("cluster_comparison_plot", height = "400px") %>%
                        shinycssloaders::withSpinner()
                ),
                shiny::column(
                    width = 6,
                    plotly::plotlyOutput("transmission_comparison_plot", height = "400px") %>%
                        shinycssloaders::withSpinner()
                ),
            ),
            br(),
            shiny::fluidRow(
                shiny::column(
                    width = 12, 
                    shiny::uiOutput(style = 'margin: 0; padding: 0', "public_data_sources")
                )
            ),
            hr(),
            # download comparison data
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    div(shiny::uiOutput("download_comparison_data_and_plots")),
                ),
            ),
        ),
    )
    
)