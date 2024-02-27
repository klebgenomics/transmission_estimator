library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)

cluster_ui <- shiny::tabPanel(
    title = "Clusters",
    # Clustering options box
    shiny::wellPanel(
        style = 'padding: 12px; margin-top: 8px;',
        shiny::fluidRow(
            shinydashboard::box(id = "cluster_options_box", width = 12,
                  shiny::fluidRow(
                      align = 'center',
                      shiny::column(width = 4,
                          shiny::numericInput("snp_threshold", "SNP distance threshold",
                                              value = DEFAULT_SNP_DIST, min = 1,
                                              max = MAX_SNP_DIST, step = 1)
                                    ),
                      shiny::column(width = 4,
                          shiny::numericInput("temporal_threshold",
                                              "Temporal distance threshold (weeks)",
                                              value = DEFAULT_TEMP_DIST, min = 1,
                                              max = MAX_TEMP_DIST, step = 1)),
                      shiny::column(width = 4, shiny::uiOutput("geo_column_picker")),
                  ),
            ),
            shiny::column(width = 6,
                actionButton(
                    style = 'font-size: 1em; margin: 4px;',
                    class = "my-button grey-button",
                    "cluster_options_button", icon = shiny::icon('minus'),
                    label = "Collapse"),
            ),
        ),
    ),
    # Cluster proportion and summary
    shiny::fluidRow(
        shiny::column( # left panel
            width = 3,
            h4(style='display: inline-block; margin: 4px 4px;', "Clusters summary"),
            div(style='display: inline-block;', 
                uiOutput("download_clusters_summary_button")
            ),
            br(), br(),
            shiny::tableOutput("clusters_summary"), # %>% withSpinner(),
            h5(shiny::textOutput("cluster_proportion")), 
            h5(shiny::textOutput("transmission_proportion")),
            shiny::conditionalPanel( # Download clusters data (if clusters exist)
                condition = "output.clusters_exist == true",
                hr(),
                h4(class="text-muted", "Download clusters data"),
                shiny::fluidRow(
                    style='margin: 6px 6px; align: center;',
                    div(style='display: inline-block',uiOutput("download_clusters_info_button")),
                    div(style='display: inline-block',uiOutput("download_clusters_data_button"))
                ),
            )
        ),
        shiny::column( # right panel - distribution plots
            width = 9,
            shiny::fluidRow(
                align = 'center',
                h4("Distribution of pairwise distances"),
                # options
                fluidRow(class = 'centered-items-row', style='height: 40px;',
                    p(style='margin: 0px 10px;', 'Bin width'),
                    div(style='margin: 16px 20px 0px 0px;',
                        shiny::numericInput("bin_width", label = NULL, value = 10, min = 1,
                                            max = 1000, width = '100px')),
                    div(style='padding: 16px 20px 0px 0px;',
                        shinyWidgets::prettySwitch("transform_y_axis", label = 'Transform Y axis', 
                                                   status = 'primary', width = '125px')),
                    shiny::conditionalPanel(condition = "input.transform_y_axis == true",
                        div(style='margin: 0px 8px; padding-top: 20px;',
                            shiny::selectInput("transformation", label = NULL, width = '100px',
                                               choices = c('log2','log10','sqrt'))),
                    ),
                ),
                # plots
                shiny::fluidRow(style='margin-top: 0px;',
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
        ),
    ), 
    hr(),
    shiny::conditionalPanel( # Cluster plots (if clusters exist)
        condition = "output.clusters_exist == true",
        # Clusters plot
        shiny::fluidRow( # header
            align = "center",
            shiny::column(
                width = 12,
                class = "centered-items-row",
                h3(style='margin: 8px 12px;', "Clusters plot"),
                div( 
                    IconButton("download_clusters_plot_button", "graph_modal"),
                ),
            ),
        ),
        shiny::fluidRow( # options
            class = 'centered-items-row',
            align = 'center',
            shiny::column(
                width=4,
                shiny::numericInput("min_cluster_size", "Minimum cases per cluster",
                                    value = 2, min = 2, max = 10, step = 1)),
            shiny::column(
                width=4,
                shiny::uiOutput("clusters_plot_colour_var")),
        ),
        shiny::fluidRow( # plot
            shiny::column(
                width = 12,
                plotly::plotlyOutput("clusters_plot", height = "600px") %>% 
                    shinycssloaders::withSpinner()
            ),
        ), 
        hr(),
        
        # Stratified cluster stats
        shiny::fluidRow( # header
            align = "center",
            shiny::column(
                width = 12,
                class = "centered-items-row",
                h3(style='margin: 8px 12px;', "Stratified cluster stats"),
                div(style='margin-right:8px;',
                    shiny::uiOutput("download_clusters_by_group_stats_button"),
                ),
                div( 
                    IconButton("download_clusters_by_group_plot_button", "graph_modal"),
                ),
            ),
        ),
        shiny::fluidRow( # options
            align = 'center',
            class = 'centered-items-row',
            shiny::column(
                width=3,
                shiny::uiOutput("cluster_stats_grouping_var")
            ),
        ),
        shiny::fluidRow( # plot
            shiny::column(
                width = 12,
                plotly::plotlyOutput("clusters_by_group_plot", height = "600px") %>% 
                    shinycssloaders::withSpinner()
            ),
        ), 
    )
)