library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)

cluster_ui <- shiny::tabPanel(
    title = "Clusters",
    shiny::fluidRow(
        style = 'margin-top: 24px;',
        shiny::column(
            width = 6,
            actionButton(
                inputId = "cluster_options_button", 
                icon = shiny::icon('minus'),
                label = "Adjust clustering options",
                class = "filter-button"),
        ),
        shinydashboard::box(id = "cluster_options_box", width = 12,
              shiny::fluidRow(
                  shiny::column(shiny::numericInput("snp_threshold", "Number of SNPs within cluster",
                                                    value = 10, min = 1, max = 1000, step = 1),
                                width = 4),
                  shiny::column(shiny::numericInput("temporal_threshold", "Number of days between isolates",
                                                    value = 14, min = 1, max = 1000, step = 1),
                                width = 4),
                  shiny::column(width = 4, shiny::uiOutput("geo_column_picker")),
              ),
        ),
    ), # End fluidRow - Cluster filters
    shiny::hr(),
    shiny::fluidRow(
        shiny::column(
            width = 3,
            h4("Clusters summary"),
            shiny::tableOutput("clusters_summary") %>% 
                shinycssloaders::withSpinner(),
            hr(),
            h5(shiny::textOutput("cluster_proportion")), #%>% 
                   # shinycssloaders::withSpinner()),
            hr(),
            h5(shiny::textOutput("transmission_proportion")) #%>% 
                   # shinycssloaders::withSpinner()),
        ),
        
        shiny::column(
            width = 9,
            shiny::fluidRow(
                align = 'center',
                h4("Distribution of pairwise distances"),
                shiny::column(
                    width = 6,
                    plotly::plotlyOutput("snp_distribution_plot") %>% shinycssloaders::withSpinner()
                ),
                shiny::column(
                    width = 6,
                    plotly::plotlyOutput("temporal_distribution_plot") %>% shinycssloaders::withSpinner()
                ),
            ),
        ),
    ), # End fluidRow - Cluster proportion and summary
    hr(),
    shiny::fluidRow(
        align = "center",
        shiny::column(width = 12, h3("Clusters plot")),
        shiny::column(
            width=4,
            shiny::numericInput("min_cluster_size", "Minimum cases per cluster",
                                value = 2, min = 2, max = 10, step = 1)),
        shiny::column(
            width=4,
            shiny::uiOutput("filter_plots_column")),
        shiny::column(
            width=4,
            shiny::uiOutput("filter_plot_options")),
        shiny::column(
            width = 12,
            plotly::plotlyOutput("clusters_plot", height = "600px") %>% shinycssloaders::withSpinner()
        ),
    ), # End fluidRow - Clusters plot
)