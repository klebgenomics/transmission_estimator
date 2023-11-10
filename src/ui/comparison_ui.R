
comparison_ui <- shiny::tabPanel(
    title = "Comparisons",
    shiny::fluidRow(
        style='margin-top: 18px;',
        shiny::column(
            width = 3,
            shinyWidgets::materialSwitch(
                inputId = "add_public_data",
                label = "Add public data",
                status = 'info'
            )
        ),
    ),
    # plots
    shiny::fluidRow(
        style='margin-top: 18px;',
        shiny::column(
            width = 12,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("comparison_cluster_prop_plot", height = "500px") %>%
                shinycssloaders::withSpinner()
        ),
        shiny::column(
            width = 12,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("comparison_transmission_prop_plot", height = "500px") %>%
                shinycssloaders::withSpinner()
        ),
    ),
    # download sensitivity data
    shiny::fluidRow(
        shiny::column(
            width = 4,
            shiny::uiOutput("download_comparison_data_button")
        )
    )
)