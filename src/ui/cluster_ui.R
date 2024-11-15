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
                          shiny::numericInput("snp_threshold", "Distance threshold",
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
            tags$div(title=CLUST_PROP_DEFINITION, 
                     h5(shiny::textOutput("cluster_proportion"))), 
            tags$div(title=gsub("\n", "", TRANSMI_PROP_DEFINITION), 
                     h5(shiny::textOutput("transmission_proportion"))),
            shiny::conditionalPanel( # Download clusters data (if clusters exist)
                condition = "output.clusters_exist == true",
                hr(),
                h4(class="text-muted", "Download clusters data"),
                shiny::fluidRow(
                    style='margin: 6px 6px;',
                    div(style='display: inline-block',uiOutput("download_clusters_info_button")),
                    div(style='display: inline-block',uiOutput("download_clusters_data_button")),
                    div(style='margin-top: 4px', uiOutput("download_clusters_graph_button"))
                ),
            )
        ),
        shiny::column( # right panel - distribution plots
            width = 9,
            shiny::fluidRow(
                align = 'center',
                h4("Distribution of pairwise distances"),
                tags$style("#bin_width, #td_bin_width {padding:4px 4px;}"),
                shiny::column(
                    width = 6,
                    # options
                    fluidRow(
                        p(style='margin: 0px 6px; display: inline-block;', 'Bin width'),
                        div(style='margin: 4px 10px 0px 0px; display: inline-block;',
                            shiny::numericInput("bin_width", label = NULL, value = 10, min = 1,
                                                max = 99, width = '50px')),
                        div(style='padding: 4px 10px 0px 0px; display: inline-block;',
                            shinyWidgets::prettySwitch("transform_y_axis", label = 'Transform Y', 
                                                       status = 'primary', width = '90px')),
                    ),
                    fluidRow(
                        div(style='height: 10px;',
                            shiny::conditionalPanel(
                                condition = "input.transform_y_axis == true",
                                shiny::selectInput("transformation", label = NULL, width = '90px',
                                                   choices = c('log2','log10','sqrt'))
                            ),
                        ),
                    ),
                    # plot
                    plotly::plotlyOutput("snp_distribution_plot", height="400px") %>% 
                        shinycssloaders::withSpinner()
                ),
                shiny::column(
                    width = 6,
                    # options
                    fluidRow(
                        p(style='margin: 0px 6px; display: inline-block;', 'Bin width'),
                        div(style='margin: 4px 10px 0px 0px; display: inline-block;',
                            shiny::numericInput("td_bin_width", label = NULL, value = 1, min = 1,
                                                max = 99, width = '50px')),
                        div(style='padding: 4px 10px 0px 0px; display: inline-block;',
                            shinyWidgets::prettySwitch("td_transform_y_axis", label = 'Transform Y', 
                                                       status = 'primary', width = '90px')),
                    ),
                    fluidRow(
                        div(style='height: 10px;',
                            shiny::conditionalPanel(
                                condition = "input.td_transform_y_axis == true",
                                shiny::selectInput("td_transformation", label = NULL, width = '90px',
                                                   choices = c('log2','log10','sqrt'))
                            ),
                        ),
                    ),
                    # plot
                    plotly::plotlyOutput("temporal_distribution_plot", height="400px") %>% 
                        shinycssloaders::withSpinner()
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
                    shinycssloaders::withSpinner(),
                p(class="text-muted", CLUSTER_PLOT_CAPTION)
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