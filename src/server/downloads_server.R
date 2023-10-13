library(shiny)

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

### CLUSTERS DATA -------------------
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
# Download clusters data
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
