library(shiny)
library(ggpubr)
library(plotly)

### INPUT OPTIONS ----------------
snp_range <- shiny::reactive({
    seq(input$snp_range[1], input$snp_range[2], by = input$interval)
})
date_range <- shiny::reactive({
    seq(input$date_range[1], input$date_range[2], by = input$interval)
})


### GET CLUSTER/TRANSMISSION PROPORTION FOR A RANGE OF THRESHOLD VALUES --------------
cluster_and_transmission_sensitivity_df <- shiny::reactive({
    shiny::req(metadata(), snp_and_epi_data(), snp_range(), date_range())
    # init empty df
    df <- data.frame(
        snp_threshold = numeric(0),
        temporal_threshold = numeric(0),
        cluster_prop = numeric(0),
        transmission_prop = numeric(0)
    )
    for (i in snp_range()){
        for (j in date_range()) {
            cluster_and_transmission_prop <- get_cluster_and_transmission_fraction(
                snp_and_epi_data(), metadata(),
                snp_distance_threshold = i, temporal_distance_threshold = j
            )
            cluster_prop <- cluster_and_transmission_prop$cluster_prop
            transmission_prop <- cluster_and_transmission_prop$transmission_prop
            # Append the results
            df <- rbind(df, data.frame(
                snp_threshold = i,
                temporal_threshold = j,
                cluster_prop = cluster_prop,
                transmission_prop = transmission_prop
                ))
        }
    } # End loop
    return(df)
})



### PLOT SENSITIVITY HEATMAPS ----------------
# Proportion of cases part of a cluster
output$cluster_prop_sensitivity_heatmap <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_heatmap(
        cluster_and_transmission_sensitivity_df(), 'cluster_prop',
        plot_title = 'Cluster proportion at different thresholds')
    plotly::ggplotly(p, height = 500)
})

# Proportion of cases due to transmission
output$transmission_prop_sensitivity_heatmap <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df())
    p <- plot_sensitivity_heatmap(
        cluster_and_transmission_sensitivity_df(), 'transmission_prop',
        plot_title = 'Transmission proportion at different thresholds')
    plotly::ggplotly(p, height = 500)
})






