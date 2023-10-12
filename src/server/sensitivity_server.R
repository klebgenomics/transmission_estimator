library(shiny)
library(ggpubr)
library(plotly)

### INPUT OPTIONS ----------------
# snp_range <- shiny::reactive({
#     seq(input$snp_range[1], input$snp_range[2])
# })
# date_range <- shiny::reactive({
#     seq(input$date_range[1], input$date_range[2]) # range of weeks
# })
snp_range <- seq(1, 25, by = 1) 
date_range <- seq(1, 52, by = 1) # weeks


### GET CLUSTER/TRANSMISSION PROPORTION FOR A RANGE OF THRESHOLD VALUES --------------
cluster_and_transmission_sensitivity_df <- shiny::reactive({
    shiny::req(metadata(), snp_and_epi_data(), snp_range, date_range)
    # init empty df
    df <- data.frame(
        snp_threshold = numeric(0),
        temporal_threshold = numeric(0),
        cluster_prop = numeric(0),
        transmission_prop = numeric(0)
    )
    for (snps in snp_range){
        for (days in date_range*7) {
            cluster_and_transmission_prop <- get_cluster_and_transmission_fraction(
                snp_and_epi_data(), metadata(),
                snp_distance_threshold = snps, temporal_distance_threshold = days
            )
            cluster_prop <- cluster_and_transmission_prop$cluster_prop
            transmission_prop <- cluster_and_transmission_prop$transmission_prop
            # Append the results
            df <- rbind(df, data.frame(
                snp_threshold = snps,
                temporal_threshold = days,
                cluster_prop = cluster_prop,
                transmission_prop = transmission_prop
                ))
        }
    } # End loop
    return(df)
})



## PLOT SENSITIVITY HEATMAPS ----------------
# Proportion of cases part of a cluster
output$cluster_prop_sensitivity_heatmap <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_heatmap(
        cluster_and_transmission_sensitivity_df(), 'cluster_prop',
        plot_title = 'Cluster proportion at different thresholds')
    plotly::ggplotly(p, height = 300)
})

# Proportion of cases due to transmission
output$transmission_prop_sensitivity_heatmap <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_heatmap(
        cluster_and_transmission_sensitivity_df(), 'transmission_prop',
        plot_title = 'Transmission proportion at different thresholds')
    plotly::ggplotly(p, height = 300)
})

### PLOT SENSITIVITY RIBBON GRAPHS -----------------------
snp_range_vals <- c(2, 5, 10, 20, 25)
temp_dist_range_vals = c(7, 14, 28, 56, 84) # days; later rescaled to weeks

# Proportion of cases in clusters by temporal threshold, for range of SNP thresholds
output$cluster_prop_temporal_plot <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_temp_dist_vs_snp_range(cluster_and_transmission_sensitivity_df(),
                                            snp_range_vals = snp_range_vals,
                                            prop_var = "cluster_prop", 
                                            y_title = "Proportion in clusters")
    plotly::ggplotly(p, height = 300)
})
# Proportion of cases in clusters by SNP threshold, for range of temporal thresholds
output$cluster_prop_SNP_plot <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_SNP_vs_temp_range(cluster_and_transmission_sensitivity_df(), 
                                            temp_dist_range_vals = temp_dist_range_vals,
                                            prop_var = "cluster_prop", 
                                            y_title = "Proportion in clusters")
    plotly::ggplotly(p, height = 300)
})
# Proportion of cases due to transmission by temporal threshold, for range of SNP thresholds
output$transmission_prop_temporal_plot <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_temp_dist_vs_snp_range(cluster_and_transmission_sensitivity_df(), 
                                                 snp_range_vals = snp_range_vals,
                                                 prop_var = "transmission_prop", 
                                                 y_title = "Proportion due to transmission")
    plotly::ggplotly(p, height = 300)
})
# Proportion of cases due to transmission by SNP threshold, for range of temporal thresholds
output$transmission_prop_SNP_plot <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_SNP_vs_temp_range(cluster_and_transmission_sensitivity_df(), 
                                            temp_dist_range_vals = temp_dist_range_vals,
                                            prop_var = "transmission_prop", 
                                            y_title = "Proportion due to transmission")
    plotly::ggplotly(p, height = 300)
})




