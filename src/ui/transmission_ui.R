library(shiny)
library(shinycssloaders)
library(DT)

cluster_ui <- shiny::tabPanel(
    title = "Transmission",
    h3("Transmission graph"),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            shinycssloaders::withSpinner( plotlyOutput("transmission_plot") )
        )
    )
)