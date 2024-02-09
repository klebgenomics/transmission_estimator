
comparison_ui <- shiny::tabPanel(
    title = "Compare estimates",
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
    hr(),
    # plots
    shiny::fluidRow(
        style='margin-top: 18px;',
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
)