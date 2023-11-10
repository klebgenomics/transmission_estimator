library(shiny)


### CLUSTERS DATA -------------------
observeEvent(epi_snp_clusters(), {
    # Download clusters summary
    output$download_clusters_summary_button <- shiny::renderUI({
        shiny::req(epi_snp_clusters(), input$snp_threshold, input$temporal_threshold)
        shiny::downloadButton("download_clusters_summary", "Download clusters summary",
                              icon = shiny::icon("download"))
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
        shiny::downloadButton("download_clusters_info", "Download clusters info",
                              icon = shiny::icon("download"))
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
        shiny::downloadButton("download_clusters_data", "Download clusters data",
                              icon = shiny::icon("download"))
    })
    output$download_clusters_data <- shiny::downloadHandler(
        filename = "clusters_data.csv",
        content = function(file) {
            d <- epi_snp_clusters() 
            write.table(d, file, row.names = FALSE, na = "", sep = ",")
        }
    )
})

### CLUSTERS STATS BY GROUP DATA -------------------
# Download grouped stats
observeEvent(input$cluster_stats_stratify_var, {
    output$download_cluster_stats_stratified_button <- shiny::renderUI({
        shiny::req(stratified_cluster_stats(), input$cluster_stats_stratify_var)
        shiny::downloadButton("download_cluster_stats_stratified", "Download grouped stats",
                              icon = shiny::icon("download"))
    })
    output$download_cluster_stats_stratified <- shiny::downloadHandler(
        filename = paste0("cluster_stats_by_", tolower(input$cluster_stats_stratify_var), ".csv"),
        content = function(file) {
            d <- stratified_cluster_stats()$stats
            write.table(d, file, row.names = FALSE, na = "", sep = ",")
        }
    )
})

### SENSITIVITY DATA ------------------
# Download sensitivity data
output$download_sensitivity_data_button <- shiny::renderUI({
    shiny::req(cluster_and_transmission_sensitivity_df())
    shiny::downloadButton("download_sensitivity_data", "Download cluster sensitivity data",
                          icon = shiny::icon("download"))
})
output$download_sensitivity_data <- shiny::downloadHandler(
    filename = "cluster_sensitivity_data.csv",
    content = function(file) {
        d <- cluster_and_transmission_sensitivity_df() %>% 
            dplyr::mutate(temporal_threshold = temporal_threshold/7) %>% # rescale to weeks
            dplyr::rename(
                "SNP threshold" = "snp_threshold",
                "Temporal threshold (weeks)" = "temporal_threshold",
                "Proportion of isolates in clusters" = "cluster_prop",
                "Proportion attributable to transmission" = "transmission_prop"
            )
        write.table(d, file, row.names = FALSE, na = "", sep = ",")
    }
)

### SITES COMPARISON DATA ------------------
output$download_comparison_data_button <- shiny::renderUI({
    shiny::req(sites_sensitivity_data())
    shiny::downloadButton("download_comparison_data", "Download comparison data",
                          icon = shiny::icon("download"))
})
output$download_comparison_data <- shiny::downloadHandler(
    filename = "site_comparison_data.csv",
    content = function(file) {
        d <- sites_sensitivity_data() %>% 
            dplyr::select(Site, tidyselect::everything()) %>% 
            dplyr::mutate(temporal_threshold = temporal_threshold/7) %>% # rescale to weeks
            dplyr::rename(
                "SNP threshold" = "snp_threshold",
                "Temporal threshold (weeks)" = "temporal_threshold",
                "Proportion of isolates in clusters" = "cluster_prop",
                "Proportion attributable to transmission" = "transmission_prop"
            )
        write.table(d, file, row.names = FALSE, na = "", sep = ",")
    }
)
