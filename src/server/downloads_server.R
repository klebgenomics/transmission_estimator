library(shiny)


### CLUSTERS TAB ---------------------------------------------------

# CLUSTERS DATA
observeEvent(epi_snp_clusters(), {
    # Download clusters summary
    output$download_clusters_summary_button <- shiny::renderUI({
        shiny::req(epi_snp_clusters(), input$snp_threshold, input$temporal_threshold)
        IconButton("download_clusters_summary", "data_dl")
    })
    output$download_clusters_summary <- shiny::downloadHandler(
        filename = "clusters_summary.csv",
        content = function(file) {
            d <- summarise_cluster2(epi_snp_clusters(), input$snp_threshold, input$temporal_threshold)
            write.table(d, file, row.names = FALSE, na = "", sep = ",")
        }
    )
    # Download clusters info
    output$download_clusters_info_button <- shiny::renderUI({
        shiny::req(epi_snp_clusters())
        shiny::downloadButton("download_clusters_info", "Info",
                              icon = shiny::icon("table"))
    })
    output$download_clusters_info <- shiny::downloadHandler(
        filename = "clusters_info.csv",
        content = function(file) {
            d <- get_cluster_info(epi_snp_clusters())
            write.table(d, file, row.names = FALSE, na = "", sep = ",")
        }
    )
    # Download full clusters data
    output$download_clusters_data_button <- shiny::renderUI({
        shiny::req(epi_snp_clusters())
        shiny::downloadButton("download_clusters_data", "Full data",
                              icon = shiny::icon("table"))
    })
    output$download_clusters_data <- shiny::downloadHandler(
        filename = "clusters_data.csv",
        content = function(file) {
            d <- epi_snp_clusters() 
            write.table(d, file, row.names = FALSE, na = "", sep = ",")
        }
    )
})

# CLUSTERS PLOT
output$download_clusters_plot <- downloadHandler(
    filename='clusters_plot.png',
    content=function(file) { download_plot(clusters_plot(), s.filename = file,
                                           width = input$plot_dl_width,
                                           height = input$plot_dl_height) }
)
observeEvent(input$download_clusters_plot_button, {
    DownloadModal(shiny::downloadButton('download_clusters_plot', 
                                        class='btn-primary'))
})

# STRATIFIED CLUSTER STATS
# Download grouped stats
output$download_clusters_by_group_stats_button <- shiny::renderUI({
    shiny::req(clusters_by_group(), input$cluster_stats_grouping_var)
    IconButton("download_clusters_by_group_stats", "data_dl")
})
output$download_clusters_by_group_stats <- shiny::downloadHandler(
    filename = reactive(paste0("cluster_stats_by_", tolower(input$cluster_stats_grouping_var), ".csv")),
    content = function(file) {
        d <- clusters_by_group()$stats
        write.table(d, file, row.names = FALSE, na = "", sep = ",")
    }
)
# Download grouped stats plot
output$download_clusters_by_group_plot <- downloadHandler(
    filename = shiny::reactive({
            paste0("cluster_stats_by_", tolower(input$cluster_stats_grouping_var), ".png")
        }),
    content=function(file) { 
        download_plot(clusters_by_group_plot(), s.filename = file,
                      width = input$plot_dl_width,
                      height = input$plot_dl_height) }
)
observeEvent(input$download_clusters_by_group_plot_button, {
    DownloadModal(shiny::downloadButton('download_clusters_by_group_plot', 
                                        class='btn-primary'))
})



### SENSITIVITY TAB --------------------------------------------------

# sensitivity data handler
output$download_sensitivity_data <- shiny::downloadHandler(
    filename = "sensitivity_data.csv",
    content = function(file) {
        d <- sensitivity_df() %>% 
            dplyr::rename(
                "SNP threshold" = "snp_threshold",
                "Temporal threshold (weeks)" = "temporal_threshold",
                "Proportion of isolates in clusters" = "cluster_prop",
                "Proportion attributable to transmission" = "transmission_prop"
            )
        write.table(d, file, row.names = FALSE, na = "", sep = ",")
    }
)
# sensitivity plots (zip) handler
output$download_sensitivity_plots <- shiny::downloadHandler(
    filename = shiny::reactive({
        paste("sensitivity_plots_", Sys.Date(), ".zip", sep = "")
    }),
    content = function(file){
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save each plot to temp dir
        download_plot(cluster_sensitivity_plot(), 
                      s.filename = file.path(temp_directory, "cluster_sensitivity_plot.png"),
                      width = input$plot_dl_width, height = input$plot_dl_height)
        download_plot(transmission_sensitivity_plot(), 
                      s.filename = file.path(temp_directory, "transmission_sensitivity_plot.png"),
                      width = input$plot_dl_width, height = input$plot_dl_height)
        # zip plots in temp dir for download
        zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
    },
    contentType = "application/zip"
)
# Download sensitivity data (rendered UI)
output$download_sensitivity_data_and_plots <- shiny::renderUI({
    shiny::req(sensitivity_df())
    div(
        h4(style='margin-right:8px; display: inline-block;', 
           class = 'text-muted', 'Download sensitivity data & plots'),
        div(style='margin-right:8px; display: inline-block;',
            # sensitivity data download button
            IconButton("download_sensitivity_data", "data_dl")
        ),
        div(style='display: inline-block;',
            # sensitivity plots modal button
            IconButton("download_sensitivity_plots_modal", "graph_modal")
        )
    )
})
# sensitivity plots modal
observeEvent(input$download_sensitivity_plots_modal, {
    DownloadModal(
        # sensitivity plots download button
        shiny::downloadButton('download_sensitivity_plots', 
                              class='btn-primary'),
        m.title = 'Download plots (ZIP)')
})

### COMPARISON TAB --------------------------------------------------

# comparison data handler
output$download_comparison_data <- shiny::downloadHandler(
    filename = "comparison_data.csv",
    content = function(file) {
        d <- all_comparison_data() %>% 
            dplyr::select(comparison_group, tidyselect::everything()) %>% 
            dplyr::rename(
                "Comparison group" = "comparison_group",
                "SNP threshold" = "snp_threshold",
                "Temporal threshold (weeks)" = "temporal_threshold",
                "Proportion of isolates in clusters" = "cluster_prop",
                "Proportion attributable to transmission" = "transmission_prop"
            )
        write.table(d, file, row.names = FALSE, na = "", sep = ",")
    }
)
# comparison plots (zip) handler
output$download_comparison_plots <- shiny::downloadHandler(
    filename = shiny::reactive({
        paste("comparison_plots_", Sys.Date(), ".zip", sep = "")
    }),
    content = function(file){
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save each plot to temp dir
        download_plot(cluster_comparison_plot(), 
                      s.filename = file.path(temp_directory, "cluster_comparison_plot.png"),
                      width = input$plot_dl_width, height = input$plot_dl_height)
        download_plot(transmission_comparison_plot(), 
                      s.filename = file.path(temp_directory, "transmission_comparison_plot.png"),
                      width = input$plot_dl_width, height = input$plot_dl_height)
        # zip plots in temp dir for download
        zip::zip(zipfile = file, files = dir(temp_directory), root = temp_directory)
    },
    contentType = "application/zip"
)
# Download comparison data (rendered UI)
output$download_comparison_data_and_plots <- shiny::renderUI({
    shiny::req(all_comparison_data())
    div(
        h4(style='margin-right:8px; display: inline-block;', 
           class = 'text-muted', 'Download comparison data & plots'),
        div(style='margin-right:8px; display: inline-block;',
            # comparison data download button
            IconButton("download_comparison_data", "data_dl")
        ),
        div(style='display: inline-block;',
            # comparison plots modal button
            IconButton("download_comparison_plots_modal", "graph_modal")
        )
    )
})
# comparison plots modal
observeEvent(input$download_comparison_plots_modal, {
    DownloadModal(
        # comparison plots download button
        shiny::downloadButton('download_comparison_plots', 
                              class='btn-primary'),
        m.title = 'Download plots (ZIP)')
})
