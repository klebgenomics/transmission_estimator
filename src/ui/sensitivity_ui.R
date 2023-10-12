library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

sensitivity_ui <- shiny::tabPanel(
    title = "Sensitivity",
    shiny::fluidRow(
        # shinydashboard::box(id = "sensitivity_options_box", width = 12,
        #                     shiny::fluidRow(
        #                         style='margin-top: 18px;',
        #                         align = 'center',
        #                         shiny::column(
        #                             width = 4,
        #                             shinyWidgets::numericRangeInput("snp_range", "Max SNP distance range",
        #                                                             value = c(0, 21), min = 0, max = 100)
        #                         ),
        #                         shiny::column(
        #                             width = 4,
        #                             shinyWidgets::numericRangeInput("date_range", "Max temporal distance range (weeks)",
        #                                                             value = c(1, 21), min = 1, max = 52)
        #                         )
        #                     ),
        # ),
    ),
    # heatmaps
    # shiny::fluidRow(
    #     shiny::column(
    #         width = 6,
    #         style='margin-bottom: 50px;',
    #         plotly::plotlyOutput("cluster_prop_sensitivity_heatmap", height = "300px") %>% 
    #             shinycssloaders::withSpinner()
    #     ),
    #     shiny::column(
    #         width = 6,
    #         style='margin-bottom: 50px;',
    #         plotly::plotlyOutput("transmission_prop_sensitivity_heatmap", height = "300px") %>% 
    #             shinycssloaders::withSpinner()
    #     ),
    # ),
    # ribbon graphs
    shiny::fluidRow(
        shiny::column(
            width = 6,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("cluster_prop_temporal_plot", height = "300px") %>%
                shinycssloaders::withSpinner()
        ),
        shiny::column(
            width = 6,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("cluster_prop_SNP_plot", height = "300px") %>%
                shinycssloaders::withSpinner()
        ),
        shiny::column(
            width = 6,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("transmission_prop_temporal_plot", height = "300px") %>%
                shinycssloaders::withSpinner()
        ),
        shiny::column(
            width = 6,
            style='margin-bottom: 50px;',
            plotly::plotlyOutput("transmission_prop_SNP_plot", height = "300px") %>%
                shinycssloaders::withSpinner()
        ),
    )
)