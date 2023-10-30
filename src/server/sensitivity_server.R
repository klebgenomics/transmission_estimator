library(shiny)
library(ggpubr)
library(plotly)

### GET CLUSTER/TRANSMISSION PROPORTION FOR A RANGE OF THRESHOLD VALUES --------------
snp_range <- seq(1, 25, by = 1) 
date_range <- seq(1, 52, by = 1) # weeks

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

### SENSITIVITY RIBBON GRAPHS -----------------------
# User-set sensitivity plot thresholds
output$date_threshold <- shiny::renderUI({
    shiny::numericInput(inputId = 'date_threshold',
                        label = 'Temporal threshold (weeks)',
                        value = 4, min = 3, max = 24)
})
output$date_threshold_range <- shiny::renderUI({
    # set intuitive colours for both sliders 
    shiny::fluidPage(shinyWidgets::setSliderColor(c("#ffc1c1", "#8b0000"),
                                                  sliderId = c(1, 2)),
        shiny::sliderInput(inputId = "date_threshold_range", 
                           label = "Temporal threshold range", 
                           min = 2, max = 25, value = c(2, 8))
    )
})
output$ext_date_threshold_range <- shiny::renderUI({
        shiny::sliderInput(inputId = "ext_date_threshold_range", 
                           label = "Extreme temporal threshold range", 
                           min = 1, max = 52, value = c(1, 52))
})

# Constrain thresholds in sensible ranges
observeEvent(c(input$date_threshold, input$date_threshold_range, input$ext_date_threshold_range), {
    min_value = input$date_threshold_range[1]
    max_value = input$date_threshold_range[2]
    if (input$date_threshold_range[1] >= input$date_threshold){
        min_value = input$date_threshold - 1
    } 
    if (input$date_threshold_range[2] <= input$date_threshold){
        max_value = input$date_threshold + 1
    }
    updateSliderInput(session, "date_threshold_range", value = c(min_value, max_value))
})
observeEvent(c(input$date_threshold, input$date_threshold_range, input$ext_date_threshold_range), {
    min_value = input$ext_date_threshold_range[1]
    max_value = input$ext_date_threshold_range[2]
    if (input$ext_date_threshold_range[1] >= input$date_threshold_range[1]){
        min_value = input$date_threshold_range[1] - 1
    } 
    if (input$ext_date_threshold_range[2] <= input$date_threshold_range[2]){
        max_value = input$date_threshold_range[2] + 1
    }
    updateSliderInput(session, "ext_date_threshold_range", value = c(min_value, max_value))
})

# set range values
sensitivity_temp_dist_range_vals <- shiny::reactive({
    c(input$ext_date_threshold_range[1],
      input$date_threshold_range[1],
      input$date_threshold,
      input$date_threshold_range[2],
      input$ext_date_threshold_range[2])
}) 

# PLOT 

# Proportion of cases in clusters by SNP threshold, for range of temporal thresholds
output$cluster_prop_SNP_plot <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df(), 
               sensitivity_temp_dist_range_vals(),
               !any(duplicated(sensitivity_temp_dist_range_vals())))
    
    p <- plot_sensitivity_SNP_vs_temp_range(cluster_and_transmission_sensitivity_df(), 
                                            temp_dist_range_vals = sensitivity_temp_dist_range_vals(),
                                            prop_var = "cluster_prop", 
                                            y_title = "Proportion in clusters",
                                            plot_title = "Proportion in clusters")
    plotly::ggplotly(p, height = 400)
})
# Proportion of cases due to transmission by SNP threshold, for range of temporal thresholds
output$transmission_prop_SNP_plot <- plotly::renderPlotly({
    shiny::req(cluster_and_transmission_sensitivity_df(), 
               sensitivity_temp_dist_range_vals(),
               !any(duplicated(sensitivity_temp_dist_range_vals())))
    p <- plot_sensitivity_SNP_vs_temp_range(cluster_and_transmission_sensitivity_df(), 
                                            temp_dist_range_vals = sensitivity_temp_dist_range_vals(),
                                            prop_var = "transmission_prop", 
                                            y_title = "Proportion due to transmission",
                                            plot_title = "Proportion due to transmission")
    plotly::ggplotly(p, height = 400)
})



