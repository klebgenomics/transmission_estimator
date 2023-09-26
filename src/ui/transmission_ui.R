library(shiny)
library(shinycssloaders)
library(DT)

cluster_ui <- shiny::tabPanel(
    title = "Clusters",
    h3("Clusters"),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            h5("Adjust cluster thresholds"), 
            shiny::fluidRow(
                shiny::column(shiny::numericInput("snp_threshold", "Number of SNPs within cluster",
                                                  value = 10, min = 1, max = 1000, step = 1),
                              width = 6),
                shiny::column(shiny::numericInput("temporal_threshold", "Number of days between isolates",
                                                  value = 14, min = 1, max = 1000, step = 1),
                              width = 6),
            ),
        ),
    ),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            shinycssloaders::withSpinner( plotlyOutput("transmission_plot") )
        )
    )
)